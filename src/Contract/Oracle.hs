{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Contract.Oracle
  ( Oracle (..),
    OracleRedeemer (..),
    oracleTokenName,
    oracleValue,
    oracleAsset,
    oracleInst,
    oracleValidator,
    oracleAddress,
    OracleSchema,
    OracleParams (..),
    runOracle,
    findOracle,
  )
where

import qualified Data.Aeson as J
import qualified Data.Map as Map
import Data.Monoid (Last(..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified Plutus.Contract as Contract
import Plutus.Contract (type (.\/))
import qualified Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

data Oracle = Oracle
  { oSymbol :: !Value.CurrencySymbol,
    oOperator :: !Ledger.PubKeyHash,
    oFee :: !Integer,
    oAsset :: !Value.AssetClass
  }
  deriving (Show, Generic, J.FromJSON, J.ToJSON, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use
  deriving (Show)

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINEABLE oracleTokenName #-}
oracleTokenName :: Value.TokenName
oracleTokenName = Value.TokenName emptyByteString

{-# INLINEABLE oracleAsset #-}
oracleAsset :: Oracle -> Value.AssetClass
oracleAsset oracle = Value.AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINEABLE oracleValue #-}
oracleValue :: Ledger.TxOut -> (Ledger.DatumHash -> Maybe Ledger.Datum) -> Maybe Integer
oracleValue o f = do
  dh <- Ledger.txOutDatum o
  Ledger.Datum d <- f dh
  PlutusTx.fromData d

{-# INLINEABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> Ledger.ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
  traceIfFalse "token missing from input" inputHasToken
    && traceIfFalse "token missing from output" outputHasToken
    && case r of
      Update ->
        traceIfFalse "operator signature missing" (Ledger.txSignedBy info $ oOperator oracle)
          && traceIfFalse "invalid output datum" validOutputDatum
      Use ->
        traceIfFalse "oracle value changed" (outputDatum == Just x)
          && traceIfFalse "fees not paid" feesPaid
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    ownInput :: Ledger.TxOut
    ownInput = case Ledger.findOwnInput ctx of
      Nothing -> traceError "oracle input missing"
      Just i -> Ledger.txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = Value.assetClassValueOf (Ledger.txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: Ledger.TxOut
    ownOutput = case Ledger.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = Value.assetClassValueOf (Ledger.txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`Ledger.findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid =
      let inVal = Ledger.txOutValue ownInput
          outVal = Ledger.txOutValue ownOutput
       in outVal `Value.geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data Oracling

instance Scripts.ScriptType Oracling where
  type DatumType Oracling = Integer
  type RedeemerType Oracling = OracleRedeemer

oracleInst :: Oracle -> Scripts.ScriptInstance Oracling
oracleInst oracle =
  Scripts.validator @Oracling
    ($$(PlutusTx.compile [||mkOracleValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Scripts.Validator
oracleValidator = Scripts.validatorScript . oracleInst

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = Ledger.scriptAddress . oracleValidator

data OracleParams = OracleParams
  { opFees :: !Integer,
    opSymbol :: !Value.CurrencySymbol,
    opToken :: !Value.TokenName
  }
  deriving (Show, Generic, J.FromJSON, J.ToJSON)

startOracle :: forall w s. Contract.HasBlockchainActions s => OracleParams -> Contract.Contract w s Text Oracle
startOracle op = do
  pkh <- Ledger.pubKeyHash <$> Contract.ownPubKey
  osc <- Contract.mapError (pack . show) (Currency.forgeContract pkh [(oracleTokenName, 1)] :: Contract.Contract w s Currency.CurrencyError Currency.OneShotCurrency)
  let cs = Currency.currencySymbol osc
      oracle =
        Oracle
          { oSymbol = cs,
            oOperator = pkh,
            oFee = opFees op,
            oAsset = Value.AssetClass (opSymbol op, opToken op)
          }
  Contract.logInfo @String $ "started oracle " ++ show oracle
  return oracle

updateOracle :: forall w s. Contract.HasBlockchainActions s => Oracle -> Integer -> Contract.Contract w s Text ()
updateOracle oracle x = do
  m <- findOracle oracle
  let c = Constraints.mustPayToTheScript x $ Value.assetClassValue (oracleAsset oracle) 1
  case m of
    Nothing -> do
      ledgerTx <- Contract.submitTxConstraints (oracleInst oracle) c
      Contract.awaitTxConfirmed $ Ledger.txId ledgerTx
      Contract.logInfo @String $ "set initial oracle value to " ++ show x
    Just (oref, o, _) -> do
      let lookups =
            Constraints.unspentOutputs (Map.singleton oref o)
              Haskell.<> Constraints.scriptInstanceLookups (oracleInst oracle)
              Haskell.<> Constraints.otherScript (oracleValidator oracle)
          tx = c Haskell.<> Constraints.mustSpendScriptOutput oref (Ledger.Redeemer $ PlutusTx.toData Update)
      ledgerTx <- Contract.submitTxConstraintsWith @Oracling lookups tx
      Contract.awaitTxConfirmed $ Ledger.txId ledgerTx
      Contract.logInfo @String $ "updated oracle value to " ++ show x

findOracle :: forall w s. Contract.HasBlockchainActions s => Oracle -> Contract.Contract w s Text (Maybe (Ledger.TxOutRef, Ledger.TxOutTx, Integer))
findOracle oracle = do
  utxos <- Map.filter f <$> Contract.utxoAt (oracleAddress oracle)
  return $ case Map.toList utxos of
    [(oref, o)] -> do
      x <- oracleValue (Ledger.txOutTxOut o) $ \dh -> Map.lookup dh $ Ledger.txData $ Ledger.txOutTxTx o
      return (oref, o, x)
    _ -> Nothing
  where
    f :: Ledger.TxOutTx -> Bool
    f o = Value.assetClassValueOf (Ledger.txOutValue $ Ledger.txOutTxOut o) (oracleAsset oracle) == 1

type OracleSchema = Contract.BlockchainActions .\/ Contract.Endpoint "update" Integer

runOracle :: OracleParams -> Contract.Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
  oracle <- startOracle op
  Contract.tell $ Last $ Just oracle
  go oracle
  where
    go :: Oracle -> Contract.Contract (Last Oracle) OracleSchema Text a
    go oracle = do
      x <- Contract.endpoint @"update"
      updateOracle oracle x
      go oracle
