module Miner.BlockMaker 
    (
        startBlockMaker
    ) where 

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan 
import Control.Concurrent.STM.TMVar
import Control.Monad
import Data.Block
import Data.Trx
import Data.ValidatedTrx
import Log.Logger
import Control.Exception
import Text.Printf
import Data.Box

loggerName :: LoggerName 
loggerName = "Miner.BlockMaker"

type BlockSize = Int 

startBlockMaker :: Maybe ThreadId      -- ThreadId of previously started solver thread
                -> BlockSize           -- Block size             
                -> BlockMakerReadBox   -- Mutable variable for reading latest block written by BlockChain 
                -> BCMReadChan         -- Channel for writing newly created block to be read by BlockChain
                -> TChan ValidatedTrx  -- Channel for reading newly validated transactions to be put into blocks
                -> TChan Trx           -- Channel to return transactions to the pool
                -> IO () 
startBlockMaker maybeSolverTId bs readBox writeBox vc tc = do 
    vts         <- replicateM bs (atomically . readTChan $ vc)
    infoM loggerName "Received enough transactions to put into a block. Will try to fetch latest block now."
    latestBlock <- atomically . takeTMVar . unBlockMakerReadBox $ readBox
    infoM loggerName "Latest block has been fetched. Will spin a new block solver now."
    maybe (spinNewSolver latestBlock vts) (\tid -> killThread tid >> spinNewSolver latestBlock vts) maybeSolverTId
    where 
        spinNewSolver :: Block -> [ValidatedTrx] -> IO ()
        spinNewSolver prevBlock vts = do
            infoM loggerName "Spinning new solver thread"
            solverTId <- forkFinally 
                            (return $ solveNewBlock prevBlock vts)
                            (either (returnTrxsToPool tc vts) (writeNewBlock writeBox))
            startBlockMaker (Just solverTId) bs readBox writeBox vc tc

returnTrxsToPool :: TChan Trx -> [ValidatedTrx] -> SomeException -> IO ()
returnTrxsToPool tc vts _ = do 
    infoM loggerName "Already running block solver thread was killed - returning transactions to the pool"
    atomically $ mapM_ (writeTChan tc) (unValidatedTrx <$> vts)

writeNewBlock :: BCMReadChan -> Block -> IO ()
writeNewBlock (BCMReadChan writeChan) block = do 
    infoM loggerName (printf "New block '%s' was solved - writing the block to BCMReadBox" (show block))
    atomically . writeTChan writeChan $ block

solveNewBlock :: Block -> [ValidatedTrx] -> Block
solveNewBlock (Block x) vts = Block (x + 1) -- TODO
