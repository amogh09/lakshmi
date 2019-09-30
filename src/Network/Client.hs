module Network.Client
    (
        publishBytes
    ) where

import Log.Logger
import Network.Socket
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Text.Printf (printf)
import System.IO
import System.Log.Logger

type Port = String
    
openHandle :: LoggerName -> HostName -> Port -> IO Handle 
openHandle loggerName host port = do 
    addrInfo:_ <- getAddrInfo Nothing (Just host) (Just port)
    sock       <- socket (addrFamily addrInfo) Stream defaultProtocol
    setSocketOption sock KeepAlive 1 
    infoM loggerName (printf "Attempting to connect to %s:%s" host port)
    connect sock (addrAddress addrInfo)
    infoM loggerName (printf "Connection to %s:%s established" host port)
    h          <- socketToHandle sock WriteMode     
    hSetBuffering h (BlockBuffering Nothing)
    return h 

publishBytes :: LoggerName -> HostName -> Port -> BS.ByteString -> IO ()
publishBytes loggerName hostName port bs = do 
    h <- openHandle loggerName hostName port 
    infoM loggerName "Sending bytes to server"
    BC.hPutStrLn h bs
    -- Explicitly call hFlush after each message, so that
    -- messages get logged immediately
    hFlush h 
    hClose h