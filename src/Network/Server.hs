module Network.Server 
    (
        startServer
    ,   echoHandler
    ,   singleMsgHandler
    ,   mkConnHandler
    ,   MsgHandler
    ,   ConnHandler
    ) where 

import Network.Socket
import System.Log.Logger
import System.IO
import Control.Monad
import Control.Concurrent
import Log.Logger
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import Network.Types

startServer :: LoggerName
            -> Port         -- Port number
            -> ConnHandler  -- Handler function
            -> IO ()
startServer loggerName port handler = do
    addr:_ <- getAddrInfo (Just tcpServerAddrInfo) Nothing (Just port)
    sock   <- socket (addrFamily addr) (addrSocketType addr) defaultProtocol
    bind sock (addrAddress addr)
    listen sock 5 
    infoM loggerName $ "Accepting connections now at port " ++ show port ++ "..."
    acceptAndManageConns loggerName sock handler

tcpServerAddrInfo :: AddrInfo 
tcpServerAddrInfo = defaultHints {addrFlags = [AI_PASSIVE]}

-- Process incoming connection requests 
acceptAndManageConns :: LoggerName -> Socket -> ConnHandler -> IO ()
acceptAndManageConns loggerName masterSock connHandler = forever $ do 
    (connSock,clientAddr) <- accept masterSock
    infoM loggerName ("Client connected: " ++ show clientAddr)
    forkIO $ connHandler connSock clientAddr

-- Closes the connection after receiving one message 
singleMsgHandler :: LoggerName -> MsgHandler -> Socket -> SockAddr -> IO () 
singleMsgHandler loggerName handler connSock clientAddr = do 
    connHandle <- socketToHandle connSock ReadMode
    hSetBuffering connHandle LineBuffering 
    messages   <- BS.hGetContents connHandle
    mapM_ (handler clientAddr) (BU.lines messages)
    hClose connHandle 
    infoM loggerName ("Client disconnected: " ++ show clientAddr)

echoHandler :: MsgHandler
echoHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ (BU.toString msg)

mkConnHandler :: IOMode -> (Handle -> IO ()) -> ConnHandler 
mkConnHandler ioMode f sock sockAddr = socketToHandle sock ioMode >>= f