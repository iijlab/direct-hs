{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.MessagePack.RPC.Client.WebSocketSpec
  ( main
  , spec
  ) where

import           Control.Applicative                      (empty, (<|>))
import           Control.Concurrent                       (threadDelay)
import           Control.Concurrent.Async                 (async,
                                                           forConcurrently,
                                                           wait)
import           Data.ByteString.Char8                    ()
import qualified Data.MessagePack                         as MsgPack
import qualified Data.Text                                ()
import qualified Network.WebSockets.Skews                 as Skews
import           System.Envy                              (FromEnv, decodeEnv,
                                                           env, fromEnv)
import           Test.Hspec
import           Text.Read                                (readMaybe)

import qualified Data.MessagePack.RPC                     as Rpc
import qualified Network.MessagePack.RPC.Client.WebSocket as Rpc


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


newtype PortNumber = PortNumber Int deriving (Eq, Show)

instance FromEnv PortNumber where
#if MIN_VERSION_envy(2, 0, 0)
  fromEnv _ = do
#else
  fromEnv = do
#endif
        mpn <- (readMaybe <$> env "MESSAGE_PACK_RPC_TEST_SERVER_PORT") <|> pure (Just 8615)
        PortNumber <$> maybe empty pure mpn


spec :: Spec
spec = describe "call" $ do
    PortNumber pn <- runIO $ either fail return =<< decodeEnv

    let host = "localhost"
    server <- runIO $ Skews.start $ Skews.Args host pn

    let withClient =
            Rpc.withClient ("ws://" ++ host ++ ":" ++ show pn) Rpc.defaultConfig

    before_ (Skews.reinit server) $ do
        it
                "when several threads call, every thread should receive corresponding response"
            $ do
                  Skews.setDefaultRequestHandler server $ \bin -> do
                      rpcMsg <- MsgPack.unpack bin
                      case rpcMsg of
                          Rpc.RequestMessage mid _methodName args ->
                              return
                                  $ Just
                                  $ MsgPack.pack
                                  $ Rpc.ResponseMessage mid
                                  $ Right
                                  $ head args
                          _ -> return Nothing

                  let requestIds = [1 .. 10]
                      expectedResponses =
                          map (Right . MsgPack.ObjectWord) requestIds

                  actualResponses <- withClient $ \client ->
                      forConcurrently requestIds
                          $ Rpc.call client "someMethod"
                          . (: [])
                          . MsgPack.ObjectWord
                  actualResponses `shouldBe` expectedResponses

        it
                "when a client is waiting for a response, any unrelated frames sent to the connection is ignored."
            $ do
                  let midSentByClient = 0
                      midNotSentByClient = 1
                      expectedResponse = MsgPack.ObjectStr "The right response"
                  actualResponse <-
                      async
                      $ withClient
                      $ \client -> Rpc.call client
                                               "someMethod"
                                               [MsgPack.ObjectStr "request"]
                  threadDelay $ 1000 * 1000 -- Wait until connection is established
                  Skews.sendToClients server
                      $ MsgPack.pack
                      $ Rpc.ResponseMessage midNotSentByClient
                      $ Right
                      $ MsgPack.ObjectStr "Wrong response"
                  Skews.sendToClients server
                      $ MsgPack.pack
                      $ Rpc.NotificationMessage
                            "methodName"
                            [MsgPack.ObjectStr "notification"]
                  Skews.sendToClients server
                      $ MsgPack.pack
                      $ Rpc.ResponseMessage midSentByClient
                      $ Right expectedResponse
                  wait actualResponse `shouldReturn` Right expectedResponse
