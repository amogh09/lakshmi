module Miner.TrxProcessor
    (
        processTrx
    ) where 

import Miner.TrxValidation
import Control.Concurrent.STM
import Data.Trx
import Log.Logger
import Text.Printf
import Data.ValidatedTrx
import Control.Monad.State

loggerName :: LoggerName
loggerName = "Miner.processTrx"

processTrx :: TChan ValidatedTrx -> TChan Trx -> StateT UTXOSet IO () 
processTrx pc tc = forever $ do    
    trx   <- liftIO . atomically . readTChan $ tc
    us    <- get
    either (handleErr trx) handleProcessed . flip runValidation us . validate $ trx
    where         
        handleErr trx err              = liftIO $
            errorM loggerName (printf "Trx '%s' failed validation with error: %s" (show trx) err)
        handleProcessed (trx', us') = do 
            put us'
            liftIO . atomically . writeTChan pc $ trx'
