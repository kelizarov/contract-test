{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Backend.API.Swap where

import qualified Plutus.PAB.Simulator                as Simulator

import           Backend.API
import           Backend.ContractStorage
import           Backend.PAB.Handlers
import           Contract.Init
import           Control.Monad                       (forM_, when)
import           Control.Monad.Freer.Error           as Error (throwError)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid                         (Last (..))
import           Plutus.Contract                     hiding (when)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import           Plutus.PAB.Types                    as Types
import           Wallet.Emulator.Types               (Wallet (..))
import           Wallet.Types                        (ContractInstanceId (..))

offerEndpoint :: WithContractStorage => OfferRequest -> Simulator.Simulation (Builtin OracleContracts) ()
offerEndpoint OfferRequest {..} = getSwapInstanceId wallet >>= runEndpoint "offer" amount

retrieveEndpoint :: WithContractStorage => RetrieveRequest -> Simulator.Simulation (Builtin OracleContracts) ()
retrieveEndpoint WithWalletRequest {..} = getSwapInstanceId wallet >>= runEndpoint "offer" ()

useEndpoint :: WithContractStorage => UseRequest -> Simulator.Simulation (Builtin OracleContracts) ()
useEndpoint WithWalletRequest {..} = getSwapInstanceId wallet >>= runEndpoint "use" ()

fundsEndpoint :: WithContractStorage => UseRequest -> Simulator.Simulation (Builtin OracleContracts) ()
fundsEndpoint WithWalletRequest {..} = getSwapInstanceId wallet >>= runEndpoint "funds" ()

getSwapInstanceId :: WithContractStorage => Wallet -> Simulator.Simulation (Builtin OracleContracts) ContractInstanceId
getSwapInstanceId wallet = do
  let throwOtherError = Error.throwError . Types.OtherError
  let errorOnNothing msg = maybe (throwOtherError msg) pure
  oracleInstanceId <- getContractId (Wallet 1) "oracle" >>= errorOnNothing "there is no oracle yet"
  oracle <- waitForLast oracleInstanceId
  savedId <- getContractId wallet "swap"
  let activateNew = do
        cid <- Simulator.activateContract wallet $ Swap oracle
        saveContractId wallet "swap" cid
        pure cid
  maybe activateNew pure savedId

runEndpoint :: ToJSON a => String -> a -> ContractInstanceId -> Simulator.Simulation (Builtin OracleContracts) ()
runEndpoint ep arg ciid = do
  err <- Simulator.callEndpointOnInstance ciid ep arg
  maybe (pure ()) (Simulator.logPretty @_ @(Builtin OracleContracts)) err


waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid = flip Simulator.waitForState cid $ \json ->
    case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

