module Miner.Miner 
    (
        startMiner
    ) where 

import qualified Miner.TrxListener as TL 
import qualified Miner.TrxProcessor as TP 
import qualified Miner.BlockMaker as BM
import qualified Data.Set as Set 
import qualified Miner.BlockChainManager as BCM
import qualified Miner.BlockPublisher as BP
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Log.Logger
import System.Exit
import Data.Block
import Data.Box
import Data.TChan
import qualified Data.BlockChain as BC 
import Network.Types
import Network.Socket

loggerName :: LoggerName 
loggerName = "Miner.Miner"

forkIOWithWait :: MVar () -> IO () -> IO ThreadId
forkIOWithWait mvar io = do 
    forkFinally io $ either 
        (\e -> errorM loggerName ("Exception: " ++ show e) >> putMVar mvar ()) 
        (\_ -> infoM loggerName "Thread finished" >> putMVar mvar ())

checkOnThreads :: MVar () -> IO () 
checkOnThreads m = do
    infoM loggerName "Checking on threads"
    _ <- takeMVar m
    errorM loggerName "Some thread has exited. Exiting program. See logs for more information." >> exitFailure

startMiner :: Port -> Port -> [(HostName, Port)] -> IO () 
startMiner wp np otherNodes = do 
    setupLogging -- TODO Move to Main

    tc    <- atomically $ newTChan 
    vc    <- atomically $ newTChan 
    bmrb  <- BlockMakerReadBox `liftM` atomically newEmptyTMVar
    bcmrc <- BCMReadChan `liftM` atomically newTChan
    bprc  <- BPReadChan `liftM` atomically newTChan

    -- Flush (discard) all messages pushed into bprc.
    -- bprc is not supposed to be used for receiving updates. It is only for pushing updates.
    -- BlockPublisher uses duplicate channels (one per thread) to receive updates.
    _     <- flushTChan (unBPReadChan bprc) 

    mv    <- newEmptyMVar
    _     <- forkIOWithWait mv $ TL.startListener tc wp -- TODO Port from config
    _     <- forkIOWithWait mv $ TP.runTrxProcessor (TP.trxProcessor vc tc) us
    _     <- forkIOWithWait mv $ BM.startBlockMaker Nothing trgt bs bmrb bcmrc bprc vc tc 
    _     <- forkIOWithWait mv $ BCM.runBCM (BCM.startBCM bmrb bcmrc) BC.empty -- TODO Initial blockchain    
    _     <- forkIOWithWait mv $ BP.startReceiver bcmrc bprc np -- TODO port from config
    _     <- forkIO $ BP.startInitiator bcmrc bprc otherNodes -- TODO port from config
    checkOnThreads mv
    
    where         
        us   = Set.empty      -- TODO fetch UTXOs from somewhere 
        bs   = 1              -- TODO BlockSize        
        trgt = 2^(256-16) - 1 -- 16 leading zeros TODO