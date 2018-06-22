{-# LANGUAGE OverloadedStrings #-}

module Network.WebSockets.MockServerSpec
  ( spec
  ) where

import           Control.Applicative           (empty, (<|>))
import           Data.ByteString.Char8         ()
import           Data.Foldable                 (toList)
import           Data.Traversable              (for)
import           Deque                         ()
import           System.Envy                   (FromEnv, decodeEnv, env,
                                                fromEnv)
import           Test.Hspec
import           Text.Read                     (readMaybe)

import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.MockServer as MockServer

-- import           Debug.Trace

newtype PortNumber = PortNumber Int deriving (Eq, Show)

instance FromEnv PortNumber where
  fromEnv = do
    mpn <- (readMaybe <$> env "WS_MOCK_SERVER_PORT") <|> pure (Just 8614)
    PortNumber <$> (maybe empty pure mpn)


spec :: Spec
spec =
  context "After running MockServer.start and starting a client" $ do
    Right (PortNumber pn) <- runIO decodeEnv

    let host = "127.0.0.1"
    server <- runIO $ MockServer.start $ MockServer.Args host pn

    let runClient = WS.runClient host pn "/"

    before_ (MockServer.reinit server) $ do
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
        MockServer.setDefaultResponse server defaultResponse

        mapM_ (MockServer.enqueResponse server) responses

        actuallyResponded <- runClient $ \conn -> do
          ress <- for requests $ \request -> do
            WS.send conn request
            WS.receive conn

          MockServer.forgetDefaultResponse server
          WS.send conn lastRequest
          {- FIXME:
          -- timeout function doesn't kill `WS.receive conn`.
          -- So I can't test the exact behaviour of forgetDefaultResponse
          resLast <- timeout 1 $ WS.receive conn
          return (ress, resLast)
          -}
          return ress

        toList <$> MockServer.recentlyReceived server `shouldReturn` requests ++ [lastRequest]
        actuallyResponded `shouldBe` responses ++ [defaultResponse, defaultResponse]

      it "sendToClients delivers the given message to all connected clients" $ do
        let msg = WS.DataMessage True True True $ WS.Binary "message"
        actuallyReceived <- runClient $ \conn1 ->
          runClient $ \conn2 -> do
            MockServer.sendToClients server msg
            (,) <$> WS.receive conn1 <*> WS.receive conn2

        actuallyReceived `shouldBe` (msg, msg)
