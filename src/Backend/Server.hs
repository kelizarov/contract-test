{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Backend.Server where

import           Backend.API
import           Backend.API.Init
import           Backend.API.Swap
import           Backend.API.Update
import           Backend.ContractStorage
import           Backend.PAB.Handlers
import           Control.Concurrent                  (forkFinally, forkIO)
import           Control.Concurrent.Availability     (Availability, available)
import           Control.Concurrent.MVar
import qualified Control.Concurrent.STM              as STM
import           Control.Monad                       (void)
import           Control.Monad.Except                (ExceptT (ExceptT))
import           Control.Monad.IO.Class              (liftIO)
import           Data.Bifunctor                      (first)
import qualified Data.ByteString.Lazy.Char8          as LBS
import           Data.Function                       ((&))
import           Data.Map                            (insert, (!?))
import qualified Network.Wai.Handler.Warp            as Warp
import           Plutus.PAB.Core                     (PABAction, PABRunner (..))
import qualified Plutus.PAB.Core                     as Core
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import           Plutus.PAB.Simulator
import           Plutus.PAB.Types                    (PABError)
import           Servant

app ::
    WithContractStorage
    => PABRunner (Builtin OracleContracts) (SimulatorState (Builtin OracleContracts))
    -> Application
app pabRunner = do
    let apiServer = Servant.hoistServer (Proxy @AppAPI) (asHandler pabRunner) handler
    Servant.serve (Proxy @AppAPI) apiServer
    where
        asHandler PABRunner{runPABAction} = Servant.Handler . ExceptT . fmap (first mapError) . runPABAction

        handler = initEndpoint :<|> updateEndpoint :<|> offerEndpoint :<|> retrieveEndpoint :<|> useEndpoint :<|> fundsEndpoint

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
    contractStorage <- liftIO $ do
          storage <- newMVar mempty
          pure ContractStorage {
            saveContractIdToStorage = \wallet endpoint instanceId -> liftIO do
              modifyMVar_ storage (pure . insert (wallet, endpoint) instanceId)
            ,
            getContractIdFromStorage = \wallet endpoint -> liftIO do
              (!? (wallet, endpoint)) <$> readMVar storage
          }
    void $ liftIO $
        forkFinally
            (Warp.runSettings warpSettings $ withContractStorage contractStorage $ app simRunner)
            (\_ -> putMVar mvar ())

    pure (mvar, liftIO $ STM.atomically $ STM.putTMVar shutdownVar ())
