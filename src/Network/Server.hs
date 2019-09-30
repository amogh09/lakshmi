module Network.Server 
    (
        startServer
    ,   echoHandler
    ,   MsgHandler
    ) where 

import Network.Socket
import System.Log.Logger
import System.IO
import Control.Monad
import Control.Concurrent
import Log.Logger
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU

type MsgHandler = SockAddr -> BS.ByteString -> IO () 

startServer :: LoggerName
            -> String      -- Port number
            -> MsgHandler  -- Handler function
            -> IO ()
startServer loggerName port handler = do    
    addr:_ <- getAddrInfo (Just tcpServerAddrInfo) Nothing (Just port)
    sock   <- socket (addrFamily addr) (addrSocketType addr) defaultProtocol
    bind sock (addrAddress addr)
    listen sock 5 
    infoM loggerName $ "Accepting connections now at port " ++ show port ++ "..."
    procRequests loggerName sock handler

tcpServerAddrInfo :: AddrInfo 
tcpServerAddrInfo = defaultHints {addrFlags = [AI_PASSIVE]}

-- Process incoming connection requests 
procRequests :: LoggerName -> Socket -> MsgHandler -> IO ()
procRequests loggerName masterSock handler = forever $ do 
    (connSock,clientAddr) <- accept masterSock
    infoM loggerName ("Client connected: " ++ show clientAddr)
    forkIO $ procMessages loggerName connSock clientAddr handler

-- Process incoming messages from a single connection
procMessages :: LoggerName -> Socket -> SockAddr -> MsgHandler -> IO () 
procMessages loggerName connSock clientAddr handler = do 
    connHandle <- socketToHandle connSock ReadMode
    hSetBuffering connHandle LineBuffering 
    messages   <- BS.hGetContents connHandle
    mapM_ (handler clientAddr) (BU.lines messages)
    hClose connHandle 
    infoM loggerName ("Client disconnected: " ++ show clientAddr)

echoHandler :: MsgHandler
echoHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ (BU.toString msg)