{-# LANGUAGE OverloadedStrings #-}

-- | MessagePack RPC Client based on WebSocket.
module Network.MessagePack.RPC.Client.WebSocket (
    -- * Config
    Config(..)
  , NotificationHandler
  , RequestHandler
  , Logger
  , Formatter
  , defaultConfig
    -- * Backend
  , Backend(..)
    -- * Client
  , URL
  , Client
  , withClient
    -- * Call and reply
  , Result
  , call
  , reply
  ) where

import qualified Data.Text                        as T
import qualified Network.WebSockets.Client        as Ws

import           Network.MessagePack.RPC.Client hiding (withClient)
import qualified Network.MessagePack.RPC.Client as RPC (withClient)

-- | URL for websocket end points.
type URL = String

-- | Executing the action in the 3rd argument with a 'Client'.
withClient
    :: URL    -- ^ URL
    -> Config -- ^ Configuration
    -> (Client -> IO a) -- ^ Action
    -> IO a
withClient url config action = Ws.withConnection url $ \conn -> do
    Ws.forkPingThread conn 30
    let backend = Backend (Ws.sendBinaryData conn)
                          (Ws.receiveData conn)
                          (Ws.sendClose conn ("Bye!" :: T.Text))
    RPC.withClient config backend action
