{-# LANGUAGE 
        GeneralizedNewtypeDeriving
    ,   MultiParamTypeClasses
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
    ,   MonadState UTXOSet
    ,   MonadError String
    )

instance (MonadValidation Trx ValidatedTrx) Validation where 
    validate = validateTrx

runValidation :: Validation a -> UTXOSet -> Either String (a, UTXOSet) 
runValidation v = runExcept . runStateT (unValidation v) 

validateTrx :: Trx -> Validation ValidatedTrx
validateTrx t = return (fromTrx t) 
    -- ValidatedTrx <$>
    -- (
    --     verifyTrxNotInBlockChain t
    -- >>= verifyInsSigs 
    -- >>= verifyInsOwnership   
    -- >>= verifyInsAreEnough 
    -- >>= verifyInsUpdateUtxos 
    -- >>= addNewUtxos     
    -- )

verifyTrxNotInBlockChain = undefined

verifyInsSigs :: Trx -> Validation Trx
verifyInsSigs = undefined -- TODO

verifyInsOwnership :: Trx -> Validation Trx
verifyInsOwnership = undefined -- TODO

verifyInsAreEnough :: Trx -> Validation Trx
verifyInsAreEnough = undefined -- TODO

verifyInsUpdateUtxos :: Trx -> Validation Trx
verifyInsUpdateUtxos = undefined -- TODO

addNewUtxos :: Trx -> Validation Trx
addNewUtxos = undefined -- TODO