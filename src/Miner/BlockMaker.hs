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
import Crypto.CryptoFuns
import qualified Data.Serialize as S
import Data.Time.Clock.POSIX

loggerName :: LoggerName 
loggerName = "Miner.BlockMaker"

type BlockSize = Int 

startBlockMaker :: Maybe ThreadId      -- ThreadId of previously started solver thread
                -> Integer             -- Block target
                -> BlockSize           -- Block size             
                -> BlockMakerReadBox   -- Mutable variable for reading latest block written by BlockChain 
                -> BCMReadChan         -- Channel for writing newly created block to be read by BlockChain
                -> BPReadChan          -- Channel for publishing new blocks to the network
                -> TChan ValidatedTrx  -- Channel for reading newly validated transactions to be put into blocks
                -> TChan Trx           -- Channel to return transactions to the pool
                -> IO () 
startBlockMaker maybeSolverTId trgt bs readBox bcmrc bprc vc tc = do 
    vts         <- replicateM bs (atomically . readTChan $ vc)
    infoM loggerName "Received enough transactions to put into a block. Will try to fetch latest block now."
    latestBlock <- atomically . takeTMVar . unBlockMakerReadBox $ readBox
    infoM loggerName "Latest block has been fetched. Will spin a new block solver now."
    maybe (spinNewSolver latestBlock vts) (\tid -> killThread tid >> spinNewSolver latestBlock vts) maybeSolverTId
    where 
        spinNewSolver :: Block -> [ValidatedTrx] -> IO ()
        spinNewSolver prevBlock vts = do
            infoM loggerName "Spinning new solver thread"
            ts        <- round <$> getPOSIXTime
            solverTId <- forkFinally 
                            (return $ solveNewBlock (Timestamp ts) trgt prevBlock vts)
                            (either (returnTrxsToPool tc vts) (writeNewBlock bcmrc bprc))
            startBlockMaker (Just solverTId) trgt bs readBox bcmrc bprc vc tc

returnTrxsToPool :: TChan Trx -> [ValidatedTrx] -> SomeException -> IO ()
returnTrxsToPool tc vts _ = do 
    infoM loggerName "Already running block solver thread was killed - returning transactions to the pool"
    atomically $ mapM_ (writeTChan tc) (toTrx <$> vts)

writeNewBlock :: BCMReadChan -> BPReadChan -> Block -> IO ()
writeNewBlock (BCMReadChan bcmrc) (BPReadChan bprc) block = do 
    infoM loggerName (printf "New block '%s' was solved - writing the block to BCMReadChan and BPReadChan" (show block))
    atomically $ (writeTChan bprc block >> writeTChan bcmrc block)

solveNewBlock :: Timestamp -> Integer -> Block -> [ValidatedTrx] -> Block
solveNewBlock ts trgt pb vts = Block bh trxs where 
    bh   = setNonce $ BlockHeader 0 trgt ts (MerkleHash . merkleHash . fmap S.encode $ trxs)
    trxs = toTrx <$> vts

-- Finds a fitting nonce and sets it on the block header
setNonce :: BlockHeader -> BlockHeader
setNonce bh
    | integerHash (S.encode bh) <= bhTarget bh = bh 
    | otherwise                                = setNonce $ bh { bhNonce = bhNonce bh + 1 }
