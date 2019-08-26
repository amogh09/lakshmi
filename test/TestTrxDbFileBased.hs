{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestTrxDbFileBased where

import TrxDbFileBased
import Control.Monad.State
import Test.QuickCheck
import Trx
import qualified Data.ByteString.UTF8 as BU

instance Arbitrary TrxInput where 
    arbitrary = do 
        ASCIIString p <- arbitrary
        Positive i <- arbitrary
        pure $ TrxInput p i

instance Arbitrary TrxOutput where 
    arbitrary = do 
        Positive v <- arbitrary
        ASCIIString a <- arbitrary
        pure $ TrxOutput v a 

instance Arbitrary Trx where 
    arbitrary = do
        ins  <- arbitrary
        outs <- arbitrary
        pure $ Trx ins outs

data MockFS = SingleFile FilePath String deriving (Show)

newtype MockFileBasedIO a = MockFileBasedIO {
        unMockFileBasedIO :: State MockFS a
    } deriving (
        Functor
    ,   Applicative
    ,   Monad 
    ,   MonadState MockFS
    )

evalMockFileBasedIO :: MockFS -> MockFileBasedIO a -> a 
evalMockFileBasedIO f m = evalState (unMockFileBasedIO m) f

instance MonadTrxDbFileBasedIO MockFileBasedIO where 
    readBytes pathReq     = do
        (SingleFile path contents) <- get
        if pathReq == path then pure (BU.fromString contents) else fail "file not found"

    writeBytes path       = put . SingleFile path . BU.toString

    liftTrxDbFileBasedIO  = lift

    catchTrxDbFileBasedIO = pure