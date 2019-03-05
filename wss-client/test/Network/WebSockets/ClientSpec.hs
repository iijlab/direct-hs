{-# LANGUAGE OverloadedStrings #-}

module Network.WebSockets.ClientSpec
  ( main
  , spec
  ) where


import           Control.Applicative        (empty, (<|>))
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text                  as T
import qualified Network.WebSockets.Skews   as Skews
import           System.Envy                (FromEnv, decodeEnv, env, fromEnv)
import           Test.Hspec
import           Text.Read                  (readMaybe)

import qualified Network.WebSockets.Client  as WS


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


newtype PortNumber = PortNumber Int deriving (Eq, Show)

instance FromEnv PortNumber where
  fromEnv = do
    mpn <- (readMaybe <$> env "WSS_CLIENT_TEST_SERVER_PORT") <|> pure (Just 8614)
    PortNumber <$> maybe empty pure mpn


spec :: Spec
spec = describe "withConnection" $ do
  PortNumber pn <- either fail return =<< runIO decodeEnv

  let host = "localhost"
  server <- runIO $ Skews.start $ Skews.Args host pn

  let withConnection = WS.withConnection ("ws://" ++ host ++ ":" ++ show pn)
      payload = "response"
      response = WS.DataMessage False False False (WS.Binary payload)
      beforeAction = do
        Skews.reinit server
        Skews.setDefaultResponse server payload

  before_ beforeAction $
    it "can send and receive messages via the connection" $ do
      actualResponse <- withConnection $ \conn -> do
        WS.sendTextData conn ("client data" :: T.Text)
        r <- WS.receive conn
        WS.sendClose conn ("Bye" :: BS.ByteString)
        return r
      actualResponse `shouldBe` response
