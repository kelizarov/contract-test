{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Backend.PAB.Handlers where

import Control.Monad (forM_, void, when)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, Result (..), fromJSON)
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import Ledger

import qualified Contract.Oracle as Oracle
import qualified Contract.Swap as Swap
import qualified Contract.Init as Init

data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty OracleContracts where
    pretty = viaShow

oracleParams :: CurrencySymbol -> Oracle.OracleParams
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000
    , Oracle.opSymbol = cs
    , Oracle.opToken  = Init.usdt
    }

handleOracleContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin OracleContracts))) effs
    ) =>
    ContractEffect (Builtin OracleContracts)
        ~> Eff effs
handleOracleContracts = handleBuiltin getSchema getContract
  where
    getSchema = \case
        Init -> endpointsToSchemas @Empty
        Oracle _ -> endpointsToSchemas @(Oracle.OracleSchema .\\ BlockchainActions)
        Swap _ -> endpointsToSchemas @(Swap.SwapSchema .\\ BlockchainActions)
    getContract = \case
        Init -> SomeBuiltin Init.initContract
        Oracle cs -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs
        Swap oracle -> SomeBuiltin $ Swap.swap oracle

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin OracleContracts) [] $
        interpret handleOracleContracts
