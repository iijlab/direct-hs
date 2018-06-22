{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.WebSockets.MockServer
  ( start
  , reinit
  , enqueRequestHandler
  , enqueResponse
  , replaceRequestHandlers
  , setDefaultRequestHandler
  , setDefaultResponse
  , forgetDefaultRequestHandler
  , forgetDefaultResponse
  , sendToClients
  , recentlyReceived
  , Server
  , RequestHandler
  , threadId
  , listeningPort
  , Args(..)
  , countConnectedClients
  ) where


import           Control.Applicative ((<|>))
import           Control.Concurrent  (ThreadId, forkIO)
import           Control.Exception   (handleJust)
import           Control.Monad       (forever, mapM_)
import qualified Data.ByteString     as B
import qualified Data.IORef          as IOR
import           Data.Monoid         (mempty)
import qualified Deque               as Q
import qualified Network.WebSockets  as WS


data Server =
  Server
    { requestHandlerQueue   :: !(IOR.IORef (Q.Deque RequestHandler))
    , defaultRequestHandler :: !(IOR.IORef (Maybe RequestHandler))
    , recentlyReceivedRef   :: !(IOR.IORef (Q.Deque WS.Message))
    , clientConnections     :: !(IOR.IORef [WS.Connection])
    , threadId              :: !ThreadId
    , listeningPort         :: !Int
    , listeningHost         :: !String
    }


type RequestHandler = WS.Message -> IO WS.Message

respondWith :: WS.Message -> RequestHandler
respondWith = const . return


-- TODO: Get port number automatically
data Args = Args
  { host       :: String
  , portNumber :: Int
  }


start :: Args -> IO Server
start (Args listeningHost listeningPort) = do
  requestHandlerQueue <- IOR.newIORef mempty
  defaultRequestHandler <- IOR.newIORef Nothing
  recentlyReceivedRef <- IOR.newIORef mempty
  clientConnections <- IOR.newIORef mempty
  threadId <- forkIO $
    WS.runServer listeningHost listeningPort $ \pc -> do
      c <- WS.acceptRequest pc
      addClientConnection clientConnections c

      ignoreConnectionClosed $ forever $ do
        m <- WS.receive c
        mbrh <- deque requestHandlerQueue
        dmbrh <- IOR.readIORef defaultRequestHandler
        case mbrh <|> dmbrh of
            Just rh -> WS.send c =<< rh m
            Nothing -> return ()
        IOR.modifyIORef recentlyReceivedRef (Q.snoc m)

  return Server {..}


addClientConnection :: IOR.IORef [WS.Connection] -> WS.Connection -> IO ()
addClientConnection ccsr c = IOR.atomicModifyIORef' ccsr (\ccs -> (c : ccs, ()))


enqueRequestHandler :: Server -> RequestHandler -> IO ()
enqueRequestHandler Server {..} rh = IOR.atomicModifyIORef' requestHandlerQueue (\q -> (Q.snoc rh q, ()))


enqueResponse :: Server -> WS.Message -> IO ()
enqueResponse s = enqueRequestHandler s . respondWith


setDefaultRequestHandler :: Server -> RequestHandler -> IO ()
setDefaultRequestHandler Server {..} = IOR.atomicWriteIORef defaultRequestHandler . Just


replaceRequestHandlers :: Server -> [RequestHandler] -> IO ()
replaceRequestHandlers Server {..} = IOR.atomicWriteIORef requestHandlerQueue . Q.fromList


setDefaultResponse :: Server -> WS.Message -> IO ()
setDefaultResponse s = setDefaultRequestHandler s . respondWith


forgetDefaultRequestHandler :: Server -> IO ()
forgetDefaultRequestHandler Server {..} = IOR.atomicWriteIORef defaultRequestHandler Nothing


forgetDefaultResponse :: Server -> IO ()
forgetDefaultResponse = forgetDefaultRequestHandler


sendToClients :: Server -> WS.Message -> IO ()
sendToClients Server {..} msg = mapM_ (`WS.send` msg) =<< IOR.readIORef clientConnections


reinit :: Server -> IO ()
reinit Server {..} = do
  IOR.atomicWriteIORef requestHandlerQueue mempty
  IOR.atomicWriteIORef defaultRequestHandler Nothing
  IOR.atomicWriteIORef recentlyReceivedRef mempty

  mapM_ (ignoreConnectionClosed . (`WS.sendClose` ("Bye" :: B.ByteString))) =<< IOR.readIORef clientConnections
  IOR.atomicWriteIORef clientConnections []


recentlyReceived :: Server -> IO (Q.Deque WS.Message)
recentlyReceived Server {..} = IOR.readIORef recentlyReceivedRef


deque :: IOR.IORef (Q.Deque a) -> IO (Maybe a)
deque qr =
  IOR.atomicModifyIORef' qr $ \q ->
    case Q.uncons q of
        Just (x, qLeft) -> (qLeft, Just x)
        _               -> (mempty, Nothing)


ignoreConnectionClosed :: IO () -> IO ()
ignoreConnectionClosed = handleJust selectClosed (const $ return ())
  where
    selectClosed :: WS.ConnectionException -> Maybe ()
    selectClosed WS.ConnectionClosed = Just ()
    selectClosed _                   = Nothing


-- | For debugging
countConnectedClients :: Server -> IO Int
countConnectedClients Server {..} = length <$> IOR.readIORef clientConnections
