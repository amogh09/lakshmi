module Miner.BlockPublisher
    (
        startInitiator
    ,   startReceiver
    ) where 

import Network.Types 
import Network.Server 
import Log.Logger 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.IO
import Miner.BlockMaker
import qualified Data.Serialize as S
import Control.Concurrent.STM 
import Control.Concurrent.STM.TMVar
import Control.Concurrent
import Network.Socket
import Network.Client
import Control.Monad
import Data.Box
import Data.Block
import Text.Printf

loggerName = "Miner.BlockPublisher"

-- Listens for blocks from the handle
blockListener :: Show hostName => hostName -> BCMReadChan -> Handle -> IO ()
blockListener hostName (BCMReadChan c) hdl = forever $ do 
    bs <- BS.hGetLine hdl
    infoM loggerName $ "Bytes '" ++ show bs ++ "' received from " ++ show hostName
    either handleErr handleBlock . S.decode $ bs
    where 
        handleErr         = errorM loggerName . ("Block could not be parsed. Error: " ++) . show
        handleBlock block = do 
            infoM loggerName $ "Block '" ++ show block ++ "' received from " ++ show hostName
            atomically . writeTChan c $ block

-- Publishes blocks to the handle
blockPublisher :: Show hostName => hostName -> BPReadChan -> Handle -> IO ()
blockPublisher hostName (BPReadChan c) hdl = forever $ do 
    block <- atomically (readTChan c)
    infoM loggerName $ "Publishing block '" ++ show block ++ "' to " ++ show hostName
    BC.hPutStrLn hdl . S.encode $ block
    hFlush hdl 
            
-- Starts a listener thread and then assumes the role of publisher
publisherAndListener :: Show hostName => BCMReadChan -> BPReadChan -> hostName -> Handle -> IO ()
publisherAndListener bcmrc bprc hostName hdl = do 
    hSetBuffering hdl LineBuffering 
    forkIO . blockListener hostName bcmrc $ hdl
    bprc' <- atomically (dupTChan . unBPReadChan $ bprc)
    blockPublisher hostName (BPReadChan bprc') hdl

-- Tries to connect to all nodes in the given list 
startInitiator :: BCMReadChan -> BPReadChan -> [(HostName, Port)] -> IO ()
startInitiator bcmrc bprc addrs = mapM_ initiateConn addrs where 
    initiateConn (h,p) = forkIO $ 
        openHandle loggerName ReadWriteMode h p >>= publisherAndListener bcmrc bprc (h ++ ":" ++ p)

-- Waits for other nodes to make a connection
startReceiver :: BCMReadChan -> BPReadChan -> Port -> IO () 
startReceiver bcmrc bprc port = 
    startServer loggerName port . mkConnHandler ReadWriteMode $ publisherAndListener bcmrc bprc
    