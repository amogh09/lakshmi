module Miner.BlockMaker 
    (
        startBlockMaker
    ,   BlockMakerReadBox (..)
    ,   BlockMakerWriteBox (..)
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

loggerName :: LoggerName 
loggerName = "Miner.BlockMaker"

type BlockSize = Int 

newtype BlockMakerReadBox  = BlockMakerReadBox  { unBlockMakerReadBox  :: TMVar Block }
newtype BlockMakerWriteBox = BlockMakerWriteBox { unBlockMakerWriteBox :: TMVar Block }

startBlockMaker :: Maybe ThreadId      -- ThreadId of previously started solver thread
                -> BlockSize           -- Block size             
                -> BlockMakerReadBox   -- Mutable variable for reading latest block written by BlockChain 
                -> BlockMakerWriteBox  -- Mutable variable for writing newly created block to be read by BlockChain
                -> TChan ValidatedTrx  -- Channel for reading newly validated transactions to be put into blocks
                -> TChan Trx           -- Channel to return transactions to the pool
                -> IO () 
startBlockMaker maybeSolverTId bs readBox writeBox vc tc = do 
    vts         <- replicateM bs (atomically . readTChan $ vc)
    latestBlock <- atomically . takeTMVar . unBlockMakerReadBox $ readBox
    maybe (spinNewSolver latestBlock vts) (\tid -> killThread tid >> spinNewSolver latestBlock vts) maybeSolverTId
    where 
        spinNewSolver :: Block -> [ValidatedTrx] -> IO ()
        spinNewSolver prevBlock vts = do
            infoM loggerName "Spinning new solver thread"
            solverTId <- forkFinally 
                            (return $ solveNewBlock prevBlock vts)
                            (either (returnTrxsToPool vts) writeNewBlock)
            startBlockMaker (Just solverTId) bs readBox writeBox vc tc
        returnTrxsToPool :: [ValidatedTrx] -> SomeException -> IO ()
        returnTrxsToPool vts _ = do 
            infoM loggerName "Block solver thread was killed - returning transactions to the pool"
            atomically $ mapM_ (writeTChan tc) (unValidatedTrx <$> vts)
        writeNewBlock :: Block -> IO ()
        writeNewBlock block = do 
            infoM loggerName "New block was solved - writing the block to BlockMakerWriteBox"
            atomically . putTMVar (unBlockMakerWriteBox writeBox) $ block

solveNewBlock :: Block -> [ValidatedTrx] -> Block
solveNewBlock prevBlock vts = undefined
