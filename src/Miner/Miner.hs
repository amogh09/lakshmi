module Miner.Miner 
    (
        startMiner
    ) where 

import qualified Miner.TrxListener as TL 
import qualified Miner.TrxProcessor as TP 
import qualified Miner.BlockMaker as BM
import qualified Data.Set as Set 
import qualified Miner.BlockChainManager as BCM
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Control.Monad
import Log.Logger
import System.Exit
import Data.Maybe (isJust)
import Data.List (find)
import Data.Block
import qualified Data.BlockChain as BC 

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

startMiner :: IO () 
startMiner = do 
    setupLogging -- TODO Move to Main

    tc   <- atomically $ newTChan 
    vc   <- atomically $ newTChan 
    bmrb <- BM.BlockMakerReadBox  `liftM` atomically newEmptyTMVar
    bmwb <- BM.BlockMakerWriteBox `liftM` atomically newEmptyTMVar

    atomically $ putTMVar (BM.unBlockMakerWriteBox bmwb) (Block 0) -- TODO Remove

    mv   <- newEmptyMVar
    _    <- forkIOWithWait mv $ TL.startListener tc "1234" -- TODO Port from config
    _    <- forkIOWithWait mv $ TP.runTrxProcessor (TP.trxProcessor vc tc) us
    _    <- forkIOWithWait mv $ BM.startBlockMaker Nothing bs bmrb bmwb vc tc 
    _    <- forkIOWithWait mv $ BCM.runBCM (BCM.startBCM bmrb bmwb) (BC.empty) -- TODO Initial blockchain
    checkOnThreads mv
    
    where         
        us = Set.empty -- TODO fetch UTXOs from somewhere 
        bs = 1         -- TODO BlockSize        