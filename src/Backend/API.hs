{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module Backend.API where

import           Backend.ContractStorage
import           Backend.PAB.Handlers
import           Control.Concurrent              (MVar, forkFinally, forkIO,
                                                  newEmptyMVar, putMVar)
import           Control.Concurrent.Availability (Availability, available)
import qualified Control.Concurrent.STM          as STM
import           Control.Monad.Except            (ExceptT (ExceptT))
import           Data.Aeson
import           Data.Bifunctor                  (first)
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Function                   ((&))
import           GHC.Generics
import qualified Network.Wai.Handler.Warp        as Warp
import           Plutus.PAB.Core                 (PABAction, PABRunner (..))
import qualified Plutus.PAB.Core                 as Core
import qualified Plutus.PAB.Effects.Contract     as Contract
import qualified Plutus.PAB.Monitoring.PABLogMsg as LM
import           Plutus.PAB.Simulator
import qualified Plutus.PAB.Simulator            as Simulator
import           Plutus.PAB.Types                (PABError,
                                                  WebserverConfig (..), baseUrl)
import           Plutus.PAB.Webserver.API        (API, NewAPI, WSAPI,
                                                  WalletProxy)
import           Plutus.PAB.Webserver.Handler    (handlerNew, handlerOld,
                                                  walletProxy,
                                                  walletProxyClientEnv)
import qualified Plutus.PAB.Webserver.WebSocket  as WS
import           Servant
import           Wallet.Emulator.Types           (Wallet (..))


data UpdateRequest = UpdateRequest
  { wallet :: Wallet
  , amount :: Integer }

data OfferRequest = OfferRequest
  { wallet :: Wallet
  , amount :: Integer }

newtype WithWalletRequest = WithWalletRequest
  { wallet :: Wallet }

instance FromJSON WithWalletRequest where
  parseJSON = withObject "WithWalletRequest" \obj ->
    WithWalletRequest <$> (fmap Wallet $ obj .: "wallet")

type RetrieveRequest = WithWalletRequest
type UseRequest = WithWalletRequest
type FundsRequest = WithWalletRequest

instance FromJSON OfferRequest where
  parseJSON = withObject "OfferRequest" \obj ->
    OfferRequest <$> (fmap Wallet $ obj .: "wallet") <*> (obj .: "amount")

instance FromJSON UpdateRequest where
  parseJSON = withObject "UpdateRequest" \obj ->
    UpdateRequest <$> (fmap Wallet $ obj .: "wallet") <*> (obj .: "amount")

type AppAPI
  =    "init"     :> Post '[JSON] ()
  :<|> "update"   :> ReqBody '[JSON] UpdateRequest   :> Post '[JSON] ()
  :<|> "offer"    :> ReqBody '[JSON] OfferRequest    :> Post '[JSON] ()
  :<|> "retrieve" :> ReqBody '[JSON] RetrieveRequest :> Post '[JSON] ()
  :<|> "use"      :> ReqBody '[JSON] UseRequest      :> Post '[JSON] ()
  :<|> "funds"    :> ReqBody '[JSON] FundsRequest    :> Post '[JSON] ()
