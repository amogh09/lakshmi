{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Miner.TrxProcessor
    (
        trxProcessor
    ,   runTrxProcessor
    ) where 

import Miner.TrxValidation
import Control.Concurrent.STM
import Data.Trx
import Log.Logger
import Text.Printf
import Data.ValidatedTrx
import Control.Monad.State

loggerName :: LoggerName
loggerName = "Miner.TrxProcessor"

newtype TrxProcessor a = TrxProcessor {
        unTrxProcessor :: StateT UTXOSet IO a
    } deriving (
        Functor
    ,   Applicative
    ,   Monad 
    ,   MonadState UTXOSet
    ,   MonadIO 
    )

runTrxProcessor :: TrxProcessor a -> UTXOSet -> IO a 
runTrxProcessor p = evalStateT (unTrxProcessor p)

trxProcessor :: TChan ValidatedTrx -> TChan Trx -> TrxProcessor ()
trxProcessor vc tc = forever $ do
    trx <- liftIO . atomically . readTChan $ tc
    us  <- get
    either (handleErr trx) handleProcessed . flip runValidation us . validate $ trx
    where         
        handleErr trx err           = liftIO $
            errorM loggerName (printf "Trx '%s' failed validation with error: %s" (show trx) err)
        handleProcessed (trx', us') = do 
            put us'
            liftIO . atomically . writeTChan vc $ trx'
