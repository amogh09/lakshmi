{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Miner.TrxProcessor
    (
        processTrx
    ) where 

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.Trx
import Log.Logger
import Text.Printf
import Data.ValidatedTrx
import Control.Monad.State
import Control.Monad.Except

loggerName :: LoggerName
loggerName = "Miner.processTrx"

processTrx :: TChan ValidatedTrx -> TChan Trx -> StateT UTXOSet IO () 
processTrx pc tc = forever $ do    
    trx   <- liftIO . atomically . readTChan $ tc
    utxos <- get
    either (handleErr trx) handleProcessed . flip runValidation utxos . validateTrx $ trx
    where         
        handleErr trx err              = liftIO $
            errorM loggerName (printf "Trx '%s' failed validation" (show trx))
        handleProcessed (trx', utxos') = do 
            put utxos'
            liftIO . atomically . writeTChan pc $ trx'

newtype Validation a = Validation {
        unValidation :: StateT UTXOSet (Except String) a
    } deriving (
        Functor
    ,   Applicative
    ,   Monad
    )

runValidation :: Validation a -> UTXOSet -> Either String (a, UTXOSet) 
runValidation v = runExcept . runStateT (unValidation v) 

validateTrx :: Trx -> Validation ValidatedTrx
validateTrx t = ValidatedTrx <$>
    (
        verifyInsSigs t    
    >>= verifyInsOwnership   
    >>= verifyInsAreEnough 
    >>= verifyInsUpdateUtxos 
    >>= addNewUtxos 
    )    

verifyInsSigs :: Trx -> Validation Trx
verifyInsSigs = undefined 

verifyInsOwnership :: Trx -> Validation Trx
verifyInsOwnership = undefined

verifyInsAreEnough :: Trx -> Validation Trx
verifyInsAreEnough = undefined

verifyInsUpdateUtxos :: Trx -> Validation Trx
verifyInsUpdateUtxos = undefined

addNewUtxos :: Trx -> Validation Trx
addNewUtxos = undefined