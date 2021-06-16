{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Contract.Init where

import Control.Monad (forM_, void, when)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, Result (..), ToJSON, fromJSON)
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import Ledger
import Ledger.Constraints
import qualified Ledger.Value as Value
import Plutus.Contract hiding (when)
import qualified Plutus.Contracts.Currency as Currency
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, handleBuiltin, type (.\\))
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (PABError (..))
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Wallet.Emulator.Types (Wallet (..), walletPubKey)
import Wallet.Types (ContractInstanceId (..))

import qualified Contract.Oracle as Oracle

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]

usdt :: TokenName
usdt = "USDT"

initContract :: Contract (Last CurrencySymbol) BlockchainActions Text ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur <-
        mapError
            (pack . show)
            ( Currency.forgeContract ownPK [(usdt, fromIntegral (length wallets) * amount)] ::
                Contract (Last CurrencySymbol) BlockchainActions Currency.CurrencyError Currency.OneShotCurrency
            )
    let cs = Currency.currencySymbol cur
        v = Value.singleton cs usdt amount
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Last $ Just cs
  where
    amount :: Integer
    amount = 100_000_000