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

loggerName = "Miner.BlockPublisher"

-- Listens for blocks from the handle
blockListener :: BCMReadChan -> Handle -> IO ()
blockListener (BCMReadChan c) hdl = forever $ 
    BS.hGetLine hdl >>= either handleErr handleBlock . S.decode
    where 
        handleErr   = errorM loggerName . ("Block could not be parsed. Error: " ++) . show
        handleBlock block = do 
            infoM loggerName $ "Block received from the network: " ++ show block
            atomically . writeTChan c $ block

-- Publishes blocks to the handle
blockPublisher :: BPReadChan -> Handle -> IO ()
blockPublisher (BPReadChan c) hdl = forever $ do 
    bs <- S.encode `liftM` atomically (readTChan c)
    infoM loggerName $ "Block received: " ++ show bs
    BC.hPutStrLn hdl bs
    hFlush hdl 
            
-- starts a publisher thread and a listener thread 
publisherAndListener :: BCMReadChan -> BPReadChan -> Handle -> IO ()
publisherAndListener bcmrc bprc hdl = do 
    hSetBuffering hdl LineBuffering 
    forkIO . blockListener bcmrc $ hdl
    atomically (dupTChan . unBPReadChan $ bprc) >>= flip blockPublisher hdl . BPReadChan

-- Tries to connect to all nodes in the given list 
startInitiator :: BCMReadChan -> BPReadChan -> [(HostName, Port)] -> IO ()
startInitiator bcmrc bprc addrs = mapM_ initiateConn addrs
    where initiateConn (h,p) = forkIO $ openHandle loggerName ReadWriteMode h p >>= publisherAndListener bcmrc bprc

-- Waits for other nodes to make a connection
startReceiver :: BCMReadChan -> BPReadChan -> Port -> IO () 
startReceiver bcmrc bprc port = 
    startServer loggerName port . mkConnHandler ReadWriteMode . publisherAndListener bcmrc $ bprc

    