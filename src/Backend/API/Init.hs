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

import           Plutus.PAB.Core                ( PABAction
                                                , PABRunner(..)
                                                )
import qualified Plutus.PAB.Simulator          as Simulator

import           Control.Monad                  ( forM_
                                                , void
                                                , when
                                                )
import           Control.Monad.Freer            ( Eff
                                                , Member
                                                , interpret
                                                , type (~>)
                                                )
import           Control.Monad.Freer.Error      ( Error )
import           Control.Monad.Freer.Extras.Log ( LogMsg )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( FromJSON
                                                , Result(..)
                                                , fromJSON
                                                )
import           Data.Monoid                    ( Last(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                  as Value
import           Plutus.Contract         hiding ( when )
import qualified Plutus.Contracts.Currency     as Currency
import           Plutus.PAB.Effects.Contract    ( ContractEffect(..) )
import           Plutus.PAB.Effects.Contract.Builtin
                                                ( type (.\\)
                                                , Builtin
                                                , SomeBuiltin(..)
                                                , endpointsToSchemas
                                                , handleBuiltin
                                                )
import           Plutus.PAB.Monitoring.PABLogMsg
                                                ( PABMultiAgentMsg )
import           Plutus.PAB.Simulator           ( SimulatorEffectHandlers )
import qualified Plutus.PAB.Simulator          as Simulator
import           Plutus.PAB.Types               ( PABError(..) )
import qualified Plutus.PAB.Webserver.Server   as PAB.Server

import           Wallet.Emulator.Types          ( Wallet(..)
                                                , walletPubKey
                                                )
import           Wallet.Types                   ( ContractInstanceId(..) )

import           Backend.PAB.Handlers
import           Contract.Init
import qualified Plutus.PAB.Effects.Contract     as Contract
import Data.Aeson


initEndpoint :: Simulator.Simulation (Builtin OracleContracts) ()
initEndpoint = do
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver. Press enter to exit."
    cidInit   <- Simulator.activateContract (Wallet 1) Init
    cs        <- waitForLast cidInit
    _         <- Simulator.waitUntilFinished cidInit

    cidOracle <- Simulator.activateContract (Wallet 1) $ Oracle cs
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle
    oracle <- waitForLast cidOracle

    forM_ wallets $ \w -> when (w /= Wallet 1) $ do
        cid <- Simulator.activateContract w $ Swap oracle
        liftIO
            $ writeFile ('W' : show (getWallet w) ++ ".cid")
            $ show
            $ unContractInstanceId cid

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid = flip Simulator.waitForState cid $ \json ->
    case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing
