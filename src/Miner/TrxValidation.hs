{-# LANGUAGE 
        GeneralizedNewtypeDeriving
    ,   FlexibleContexts
    ,   MultiParamTypeClasses
    ,   FunctionalDependencies 
#-}

module Miner.TrxValidation
    (
        runValidation
    ,   module Miner.Validation
    ) where

import Miner.Validation
import Data.Trx
import Data.ValidatedTrx
import Control.Monad.State 
import Control.Monad.Except

newtype Validation a = Validation {
       unValidation :: StateT UTXOSet (Except String) a
    } deriving (
        Functor
    ,   Applicative
    ,   Monad
    )

instance (MonadValidation Trx ValidatedTrx) Validation where 
    validate = validateTrx

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