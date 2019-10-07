module Miner.BlockPublisher
    (
        startBlockPublisher
    ,   startTestClient
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

blockListner :: BCMReadChan -> Listener
blockListner (BCMReadChan c) hdl = forever $ 
    BS.hGetLine hdl >>= either handleErr handleBlock . S.decode
    where 
        handleErr   = errorM loggerName . ("Block could not be parsed. Error: " ++) . show
        handleBlock block = do 
            infoM loggerName $ "Block received from the network: " ++ show block
            atomically . writeTChan c $ block

blockPublisher :: BPReadChan -> Listener
blockPublisher (BPReadChan c) hdl = forever $ do 
    infoM loggerName "Waiting for block"
    bs <- S.encode `liftM` atomically (readTChan c)    
    infoM loggerName $ "Block received: " ++ show bs
    BC.hPutStrLn hdl bs
    hFlush hdl 
        
connHandler :: Listener -> Listener -> ConnHandler 
connHandler blockListener blockPublisher sock sockAddr = do 
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl LineBuffering
    forkIO $ blockListener hdl
    blockPublisher hdl
    
startBlockPublisher :: BPReadChan -> BCMReadChan -> Port -> IO () 
startBlockPublisher bprc bcmrc port = 
    startServer loggerName port $ connHandler (blockListner bcmrc) (blockPublisher bprc)

testListener :: Listener
testListener hdl = forever $ do
    infoM loggerName "Waiting for block..."
    BS.hGetLine hdl >>= either handleErr handleBlock . S.decode
    where 
        handleErr   = errorM loggerName . ("Block could not be parsed. Error: " ++) . show
        handleBlock :: Block -> IO ()
        handleBlock block@(Block i) = do
            infoM loggerName ("Received block: " ++ show block)
            BC.hPutStrLn hdl . S.encode . Block $ i + 1
            hFlush hdl 

startTestClient :: IO () 
startTestClient = do 
    setupLogging
    hdl <- openHandle loggerName ReadWriteMode "localhost" "1235" 
    hSetBuffering hdl LineBuffering 
    testListener hdl
