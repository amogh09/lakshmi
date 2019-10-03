{-# LANGUAGE FunctionalDependencies #-}

module Miner.Validation 
    (
        MonadValidation (..)
    ) where

class Monad m => MonadValidation a b m | m -> a b where 
    validate :: a -> m b
