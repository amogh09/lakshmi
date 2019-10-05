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

forkIOWithWait :: IO () -> IO (MVar ())
forkIOWithWait io = do 
    mvar <- newEmptyMVar 
    forkFinally io $ either (\e -> errorM loggerName ("Exception: " ++ show e) >> putMVar mvar ()) (\_ -> putMVar mvar ())
    return mvar 

checkOnThreads :: [MVar ()] -> IO () 
checkOnThreads ts = do
    infoM loggerName "Checking on threads"
    statuses <- mapM tryTakeMVar ts 
    case find isJust statuses of 
        Nothing -> threadDelay (10*1000*1000) >> checkOnThreads ts 
        _       -> errorM loggerName "Some thread has exited. Exiting program." >> exitFailure

startMiner :: IO () 
startMiner = do 
    setupLogging -- TODO Move to Main

    tc   <- atomically $ newTChan 
    vc   <- atomically $ newTChan 
    bmrb <- BM.BlockMakerReadBox  `liftM` atomically newEmptyTMVar
    bmwb <- BM.BlockMakerWriteBox `liftM` atomically newEmptyTMVar

    atomically $ putTMVar (BM.unBlockMakerWriteBox bmwb) (Block 0)

    mv1  <- forkIOWithWait $ TL.startListener tc "1234" -- TODO Port from config
    mv2  <- forkIOWithWait $ TP.runTrxProcessor (TP.trxProcessor vc tc) us
    mv3  <- forkIOWithWait $ BM.startBlockMaker Nothing bs bmrb bmwb vc tc 
    mv4  <- forkIOWithWait $ BCM.runBCM (BCM.startBCM bmrb bmwb) (BC.empty) -- TODO Initial blockchain
    checkOnThreads [mv1,mv2,mv3,mv4]
    
    where         
        us = Set.empty -- TODO fetch UTXOs from somewhere 
        bs = 1         -- TODO BlockSize        