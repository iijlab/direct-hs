{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.WebSockets.MockServer
  ( start
  , reinit
  , enqueRequestHandler
  , enqueRequestHandlers
  , enqueResponse
  , enqueResponses
  , replaceRequestHandlers
  , sendToClients
  , recentlyReceived
  , Server
  , RequestHandler
  , threadId
  , listeningPort
  , Args(..)
  ) where


import           Control.Concurrent (ThreadId, forkIO)
import           Control.Exception  (handleJust)
import           Control.Monad      (forever, mapM_)
import qualified Data.ByteString    as B
import qualified Data.IORef         as IOR
import qualified Network.WebSockets as WS


data Server =
  Server
    { requestHandlerQueue :: !(IOR.IORef [RequestHandler])
    , recentlyReceivedRef :: !(IOR.IORef [WS.Message])
    , clientConnections   :: !(IOR.IORef [WS.Connection])
    , threadId            :: !ThreadId
    , listeningPort       :: !Int
    , listeningHost       :: !String
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
  requestHandlerQueue <- IOR.newIORef []
  recentlyReceivedRef <- IOR.newIORef []
  clientConnections <- IOR.newIORef []
  threadId <- forkIO $
    WS.runServer listeningHost listeningPort $ \pc -> do
      c <- WS.acceptRequest pc
      addClientConnection clientConnections c

      ignoreConnectionClosed $ forever $ do
        m <- WS.receive c
        mbrh <- deque requestHandlerQueue
        case mbrh of
            Just rh -> WS.send c =<< rh m
            Nothing -> return ()
        IOR.modifyIORef recentlyReceivedRef (++ [m])

  return Server {..}


addClientConnection :: IOR.IORef [WS.Connection] -> WS.Connection -> IO ()
addClientConnection ccs c = IOR.modifyIORef' ccs (c :)


enqueRequestHandler :: Server -> RequestHandler -> IO ()
enqueRequestHandler Server {..} rh = IOR.modifyIORef requestHandlerQueue (++ [rh])


enqueRequestHandlers :: Server -> [RequestHandler] -> IO ()
enqueRequestHandlers Server {..} rhs = IOR.modifyIORef requestHandlerQueue (++ rhs)


enqueResponse :: Server -> WS.Message -> IO ()
enqueResponse s = enqueRequestHandler s . respondWith


enqueResponses :: Server -> [WS.Message] -> IO ()
enqueResponses s = enqueRequestHandlers s . map respondWith


replaceRequestHandlers :: Server -> [RequestHandler] -> IO ()
replaceRequestHandlers Server {..} = IOR.writeIORef requestHandlerQueue


sendToClients :: Server -> WS.Message -> IO ()
sendToClients Server {..} msg = mapM_ (`WS.send` msg) =<< IOR.readIORef clientConnections


reinit :: Server -> IO ()
reinit Server {..} = do
  IOR.writeIORef requestHandlerQueue []
  IOR.writeIORef recentlyReceivedRef []

  mapM_ (ignoreConnectionClosed . (`WS.sendClose` ("Bye" :: B.ByteString))) =<< IOR.readIORef clientConnections
  IOR.writeIORef clientConnections []


recentlyReceived :: Server -> IO [WS.Message]
recentlyReceived Server {..} = IOR.readIORef recentlyReceivedRef


deque :: IOR.IORef [a] -> IO (Maybe a)
deque q =
  IOR.atomicModifyIORef q $
    \case
          (x : xs) -> (xs, Just x)
          [] -> ([], Nothing)


ignoreConnectionClosed :: IO () -> IO ()
ignoreConnectionClosed = handleJust selectClosed (const $ return ())
  where
    selectClosed :: WS.ConnectionException -> Maybe ()
    selectClosed WS.ConnectionClosed = Just ()
    selectClosed _                   = Nothing
