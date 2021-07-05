{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module GenericServer where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Extras.Log
import           Control.Monad.IO.Class
import           Data.Map                                (Map, delete, insert,
                                                          (!))
import           Data.Time
import           GHC.Conc.Sync
import           Plutus.PAB.Core
import           Plutus.PAB.Core.ContractInstance.STM
import           Plutus.PAB.Effects.Contract
import           Plutus.PAB.Effects.Contract.ContractExe
import           Plutus.PAB.Webserver.Types
import           Wallet.Types
import           Web.Scotty

import           Cardano.BM.Data.Trace
import           Cardano.BM.Setup
import           Control.Exception.Base
import           Data.Aeson
import           Data.Function                           ((&))
import           Data.Text                               (Text, unpack)
import qualified Data.Text.IO                            as TIO
import           Data.Yaml
import           GHC.Generics
import           Plutus.PAB.App
import           Plutus.PAB.Monitoring.Config
import           Plutus.PAB.Monitoring.PABLogMsg
import           Plutus.PAB.Monitoring.Util
import           Plutus.PAB.Types
import           System.IO.Error
import           Wallet.Emulator.Wallet
import           Web.Scotty.Trans                        (ActionT, stringError)

data Message = Message Request (MVar Value)

data Request
  = Install InstallRequest
  | Activate ActivateRequest
  | Call CallRequest

data InstallRequest = InstallRequest
  { name :: Text
  , tag  :: Text }
  deriving (Generic, FromJSON)

data ActivateRequest = ActivateRequest
  { name   :: Text
  , tag    :: Text
  , wallet :: Integer }
  deriving (Generic, FromJSON)

data CallRequest = CallRequest
  { instanceId :: ContractInstanceId
  , endpoint   :: String
  , payload    :: Value }
  deriving (Generic, FromJSON)

pab :: IO (Chan Message)
pab = do
  chan <- newChan
  logConfig <- defaultConfig
  (trace :: Trace IO (PrettyObject (AppMsg ContractExe)), _) <- setupTrace_ logConfig "pab"
  let trace' = convertLog PABMsg $ convertLog PrettyObject trace
  migrate trace' (DbConfig "pab-core.db" 10)
  config <- liftIO $ decodeFileThrow "plutus-pab.yaml"
  void $ forkIO do
    void $ runApp SqliteBackend trace' config $ forever do
      liftIO (readChan chan) >>= \case
          Message (Install req) back  -> installHandler req back
          Message (Activate req) back -> activateHandler req back
          Message (Call req) back     -> callHandler req back

  pure chan
    where
      installHandler InstallRequest {..} back = do
        installContract @ContractExe (ContractExe $ pathToExecContract name tag)
        makeResponse back ("ok" :: Text)
      activateHandler ActivateRequest {..} back = do
        result <- activateContract (Wallet wallet) (ContractExe $ pathToExecContract name tag)
        makeResponse back result
      callHandler CallRequest {..} back = do
        mbError <- Plutus.PAB.Core.callEndpointOnInstance instanceId endpoint payload
        maybe (makeResponse back ("ok" :: Text)) (makeResponse back) mbError

contractsDirectory :: Text
contractsDirectory = "contracts"

pathToExecContract :: Text -> Text -> FilePath
pathToExecContract name tag = unpack $ "." <> "/" <> contractsDirectory <> "/" <> name <> "/" <> tag <> "-exe"

serverApp :: IO ()
serverApp = do
  chan <- pab
  let catchBodyAndPack ::
          FromJSON b
          => (RoutePattern -> ActionM () -> ScottyM ())
          -> RoutePattern
          -> (b -> Request)
          -> ScottyM ()
      catchBodyAndPack method path f =
        method path do parsedBody >>= waitResponseAndSend chan . f

  scotty 5000 do
    catchBodyAndPack post "/install" Install
    catchBodyAndPack post "/activate" Activate
    catchBodyAndPack post "/call" Call


parsedBody :: FromJSON a => ActionM a
parsedBody = do
  content <- body
  eitherDecode content & \case
    Left err -> Web.Scotty.raise $ stringError $ "Parsing failed: " <> err
    Right ok -> pure ok

waitResponseAndSend :: Chan Message -> Request -> ActionM ()
waitResponseAndSend chan ev =
  liftIO (do
    box <- newEmptyMVar
    writeChan chan $ Message ev box
    readMVar box) >>= Web.Scotty.json

makeResponse :: MonadIO m => ToJSON a => MVar Value -> a -> m ()
makeResponse back = liftIO . putMVar back . Data.Aeson.toJSON
