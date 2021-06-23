{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE IncoherentInstances #-}
module Backend.API.Update where

import           Backend.API
import           Backend.ContractStorage
import           Backend.PAB.Handlers
import           Control.Monad.Freer.Error           as Error (throwError)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    as Types

updateEndpoint ::  WithContractStorage => UpdateRequest -> Simulator.Simulation (Builtin OracleContracts) ()
updateEndpoint UpdateRequest {..} = do
    let logString = Simulator.logString @(Builtin OracleContracts)
    oracleInstanceId <- getContractId wallet "oracle" >>= maybe (Error.throwError $ Types.OtherError "failed to get instance ID") pure
    err <- Simulator.callEndpointOnInstance oracleInstanceId "update" amount
    maybe (logString "Updated successfully") (Simulator.logPretty @_ @(Builtin OracleContracts)) err
