{-# LANGUAGE OverloadedStrings #-}

module Network.WebSockets.SkewsSpec
  ( spec
  ) where

import           Control.Applicative      (empty, (<|>))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, replicateConcurrently,
                                           replicateConcurrently_, wait)
import           Data.ByteString.Char8    ()
import           Data.Foldable            (toList)
import           Data.Traversable         (for)
import           Deque                    ()
import           System.Envy              (FromEnv, decodeEnv, env, fromEnv)
import           Test.Hspec
import           Text.Read                (readMaybe)

import qualified Network.WebSockets       as WS
import qualified Network.WebSockets.Skews as Skews

-- import           Debug.Trace

newtype PortNumber = PortNumber Int deriving (Eq, Show)

instance FromEnv PortNumber where
  fromEnv = do
    mpn <- (readMaybe <$> env "SKEWS_TEST_PORT") <|> pure (Just 8614)
    PortNumber <$> maybe empty pure mpn


spec :: Spec
spec =
  context "After running Skews.start and starting a client" $ do
    Right (PortNumber pn) <- runIO decodeEnv

    let host = "127.0.0.1"
    server <- runIO $ Skews.start $ Skews.Args host pn

    let runClient = WS.runClient host pn "/"

    before_ (Skews.reinit server) $ do
      it "after enqueResponses and setDefaultResponse, responds with the given responses and records request" $ do
        let requests =
              [ WS.DataMessage True True True $ WS.Binary "client1"
              , WS.DataMessage True True True $ WS.Binary "client2"
              , WS.DataMessage True True True $ WS.Binary "client3"
              , WS.DataMessage True True True $ WS.Binary "client4"
              , WS.DataMessage True True True $ WS.Binary "client5"
              ]
            -- Close code 1000: normal closure: https://tools.ietf.org/html/rfc6455#section-7.4
            lastRequest = WS.ControlMessage $ WS.Close 1000 "bye by client"
            responses =
              [ WS.DataMessage True True True $ WS.Binary "response1"
              , WS.DataMessage True True True $ WS.Binary "response2"
              , WS.DataMessage True True True $ WS.Binary "response3"
              ]

            defaultResponse = WS.DataMessage True True True (WS.Binary "default response")
        Skews.setDefaultResponse server defaultResponse

        mapM_ (Skews.enqueResponse server) responses

        actuallyResponded <- runClient $ \conn -> do
          ress <- for requests $ \request -> do
            WS.send conn request
            WS.receive conn

          Skews.forgetDefaultResponse server
          WS.send conn lastRequest
          {- FIXME:
          -- timeout function doesn't kill `WS.receive conn`.
          -- So I can't test the exact behaviour of forgetDefaultResponse
          resLast <- timeout 1 $ WS.receive conn
          return (ress, resLast)
          -}
          return ress

        toList <$> Skews.recentlyReceived server `shouldReturn` requests ++ [lastRequest]
        actuallyResponded `shouldBe` responses ++ [defaultResponse, defaultResponse]

      it "sendToClients delivers the given message to all connected clients" $ do
        let msg = WS.DataMessage True True True $ WS.Binary "message"
            clientCount = 10
            clientAction = runClient $ \conn -> do
              threadDelay $ 100 * 1000
              WS.receive conn

        actuallyReceived <- async $ replicateConcurrently clientCount clientAction
        threadDelay $ 50 * 1000
        Skews.sendToClients server msg

        wait actuallyReceived `shouldReturn` replicate clientCount msg

      it "can record requests sent concurrently" $ do
        let msg = WS.DataMessage True True True $ WS.Binary "client message"
            clientCount = 10
            clientAction = runClient (`WS.send` msg)
        replicateConcurrently_ clientCount clientAction
        threadDelay $ 50 * 1000
        toList <$> Skews.recentlyReceived server `shouldReturn` replicate clientCount msg
