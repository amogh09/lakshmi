{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Miner.BlockChainManager
    (
        startBCM
    ,   runBCM
    ) where

import Data.BlockChain
import Log.Logger 
import Miner.BlockMaker
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Data.Box

loggerName :: LoggerName 
loggerName = "Miner.BlockChainManager"

ensureTMVar :: TMVar a -> a -> STM () 
ensureTMVar v x = do 
    f <- isEmptyTMVar v 
    if f 
        then putTMVar v x
        else swapTMVar v x >> return ()

newtype BlockChainManager a = BlockChainManager {
        unBlockChainManager :: StateT BlockChain IO a 
    } deriving (
        Functor
    ,   Applicative 
    ,   Monad 
    ,   MonadState BlockChain
    ,   MonadIO
    )

runBCM :: BlockChainManager a -> BlockChain -> IO a 
runBCM m = evalStateT (unBlockChainManager m)

startBCM :: BlockMakerReadBox -- To write new latest block to block solver
         -> BCMReadChan       -- To read new latest block from block solver and other nodes
         -> BlockChainManager ()
startBCM (BlockMakerReadBox rb) (BCMReadChan bcmChan) = forever $ do 
    newBlock <- liftIO . atomically . readTChan $ bcmChan
    liftIO $ infoM loggerName ("Received new block: " ++ show newBlock)
    -- TODO Validate block here or in BlockListener
    modify (putBlock newBlock)
    get >>= liftIO . atomically . ensureTMVar rb . bcHead