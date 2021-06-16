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

module Main where

import qualified Plutus.PAB.Simulator                as Simulator
import           Control.Monad.IO.Class              (MonadIO (..))
import Control.Concurrent.Availability
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import Control.Monad 

import Backend.PAB.Handlers
import qualified Backend.Server as Server


main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    tk <- newToken
    (_, shutdown) <- Server.startServer 8080 tk
    void $ liftIO getLine
    shutdown
    pure ()

