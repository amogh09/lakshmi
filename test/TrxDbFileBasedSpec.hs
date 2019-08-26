{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TrxDbFileBasedSpec where

import TrxDbFileBased
import Control.Monad.State
import Test.QuickCheck
import Trx
import qualified Data.ByteString.UTF8 as BU
import Test.Hspec
import Data.Either (isRight, isLeft)
import System.IO.Error (isDoesNotExistError)

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

data MockFS = EmptyDir
            | SingleFile FilePath String 
            deriving (Show)

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
    readBytes pathReq = do
        dir <- get
        case dir of 
            EmptyDir                 -> fail "file not found"
            SingleFile path contents -> if pathReq == path 
                                            then pure (BU.fromString contents) 
                                            else fail "file not found"

    writeBytes path = put . SingleFile path . BU.toString

    liftTrxDbFileBasedIO = lift

    catchTrxDbFileBasedIO = pure

spec :: Spec 
spec = do 
    describe "TrxDbFileBased" $ do 
        it "is able to read the transactions it wrote" $ do 
            property $ \ts -> 
                let env = TrxDbFileBasedEnv "dir/trx.dat"
                    res = evalMockFileBasedIO EmptyDir . runTrxDbFileBased env $ do
                            writeAllTrx ts
                            ts' <- readAllTrx
                            pure (ts == ts')
                in  case res of 
                        Right True -> True 
                        _          -> False

        it "fails with error if non-existing file is attempted to be read" $ 
            let env = TrxDbFileBasedEnv "dir/trx.dat"
                res = runTrxDbFileBased env $ readAllTrx
            in  res >>= (`shouldSatisfy` \r -> 
                    case r of 
                        Left (TrxDbErrorIO env e) -> isDoesNotExistError e
                        _                         -> False)