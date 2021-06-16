{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Backend.Server where

import Servant
import Data.Aeson
import qualified Network.Wai.Handler.Warp        as Warp
import           Data.Function                   ((&))
import           Control.Concurrent.Availability (Availability, available)

import qualified Control.Concurrent.STM          as STM
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Control.Concurrent              (MVar, forkFinally, forkIO, newEmptyMVar, putMVar)

import           Plutus.PAB.Core                 (PABAction, PABRunner (..))
import qualified Plutus.PAB.Core                 as Core
import qualified Plutus.PAB.Effects.Contract     as Contract
import qualified Plutus.PAB.Monitoring.PABLogMsg as LM
import           Plutus.PAB.Types                (PABError, WebserverConfig (..), baseUrl)
import           Plutus.PAB.Webserver.API        (API, NewAPI, WSAPI, WalletProxy)
import           Plutus.PAB.Webserver.Handler    (handlerNew, handlerOld, walletProxy, walletProxyClientEnv)
import qualified Plutus.PAB.Webserver.WebSocket  as WS
import           Control.Monad.Except            (ExceptT (ExceptT))
import           Data.Bifunctor                  (first)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad                   (void)
import qualified Control.Monad.Freer.Extras.Log as Log
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import Backend.PAB.Handlers
import Plutus.PAB.Simulator
import Backend.API.Init
import qualified Plutus.PAB.Simulator          as Simulator



type AppAPI = "init" :> Post '[JSON] ()

handler :: Simulator.Simulation (Builtin OracleContracts) ()
handler = initEndpoint

app ::
    PABRunner (Builtin OracleContracts) (SimulatorState (Builtin OracleContracts))
    -> Application
app pabRunner = do
    let apiServer = Servant.hoistServer (Proxy @AppAPI) (asHandler pabRunner) handler
    Servant.serve (Proxy @AppAPI) apiServer
    where
        asHandler PABRunner{runPABAction} = Servant.Handler . ExceptT . fmap (first mapError) . runPABAction

        mapError :: PABError -> Servant.ServerError
        mapError e = Servant.err500 { Servant.errBody = LBS.pack $ show e }

startServer ::
    Int 
    -> Availability
    -> PABAction (Builtin OracleContracts) (SimulatorState (Builtin OracleContracts)) (MVar (), PABAction (Builtin OracleContracts) (SimulatorState (Builtin OracleContracts)) ())
startServer port availability = do
    simRunner <- Core.pabRunner
    shutdownVar <- liftIO $ STM.atomically $ STM.newEmptyTMVar @()
    mvar <- liftIO newEmptyMVar

    let shutdownHandler :: IO () -> IO ()
        shutdownHandler doShutdown = void $ forkIO $ do
            STM.atomically $ STM.takeTMVar shutdownVar
            putStrLn "webserver: shutting down"
            doShutdown
        warpSettings :: Warp.Settings
        warpSettings = Warp.defaultSettings
            & Warp.setPort port
            & Warp.setInstallShutdownHandler shutdownHandler
            & Warp.setBeforeMainLoop (available availability)
    void $ liftIO $
        forkFinally
            (Warp.runSettings warpSettings $ app simRunner)
            (\_ -> putMVar mvar ())

    pure (mvar, liftIO $ STM.atomically $ STM.putTMVar shutdownVar ())