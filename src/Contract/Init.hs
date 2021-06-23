{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Contract.Init where

import           Control.Monad             (forM_, when)
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value              as Value
import           Plutus.Contract           hiding (when)
import qualified Plutus.Contracts.Currency as Currency
import           Wallet.Emulator.Types     (Wallet (..), walletPubKey)


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
