{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Backend.API.Init where

import qualified Plutus.PAB.Simulator                as Simulator

import           Control.Monad                       (forM_, when)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Monoid                         (Last (..))
import           Plutus.Contract                     hiding (when)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import           Wallet.Emulator.Types               (Wallet (..))
import           Wallet.Types                        (ContractInstanceId (..))

import           Backend.ContractStorage
import           Backend.PAB.Handlers
import           Contract.Init
import           Data.Aeson


initEndpoint :: WithContractStorage => Simulator.Simulation (Builtin OracleContracts) ()
initEndpoint = do
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver. Press enter to exit."
    let w1 = Wallet 1
    cidInit   <- Simulator.activateContract w1 Init
    saveContractId w1 "init" cidInit
    cs        <- waitForLast cidInit
    _         <- Simulator.waitUntilFinished cidInit

    cidOracle <- Simulator.activateContract w1 $ Oracle cs
    saveContractId w1 "oracle" cidOracle
    -- oracle <- waitForLast cidOracle

    -- forM_ wallets $ \w -> when (w /= w1) $ do
    --     cid <- Simulator.activateContract w $ Swap oracle
    --     saveContractId w "swap" cid


waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid = flip Simulator.waitForState cid $ \json ->
    case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing
