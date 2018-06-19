{-# LANGUAGE OverloadedStrings #-}

-- | MessagePack RPC Client based on WebSocket.
module Network.MessagePack.Async.Client.WebSocket (
    withClient
  , module Network.MessagePack.Async.Client
  ) where

import qualified Data.Text as T
import qualified Network.WebSockets.Client as Ws

import           Network.MessagePack.Async.Client hiding (withClient)
import qualified Network.MessagePack.Async.Client as Rpc (withClient)

-- | Executing the action in the 3rd argument with a 'Client'.
withClient :: String -- ^ URL
           -> Config -- ^ Configuration
           -> (Client -> IO a) -- ^ Action
           -> IO a
withClient url config action =
  Ws.withConnection url $ \conn -> do
    Ws.forkPingThread conn 30
    let backend = Backend (Ws.sendBinaryData conn) (Ws.receiveData conn) (Ws.sendClose conn ("Bye!" :: T.Text))
    Rpc.withClient config backend action
