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

loggerName :: LoggerName 
loggerName = "Miner.BlockChainManager"

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

startBCM :: BlockMakerReadBox   -- To write new latest block to block solver
         -> BlockMakerWriteBox  -- To read new latest block from block solver
         -> BlockChainManager ()
startBCM (BlockMakerReadBox rb) (BlockMakerWriteBox wb) = forever $ do 
    newBlock <- liftIO . atomically . takeTMVar $ wb
    liftIO $ infoM loggerName ("Received new block: " ++ show newBlock)
    modify (putBlock newBlock)
    get >>= liftIO . atomically . putTMVar rb . bcHead