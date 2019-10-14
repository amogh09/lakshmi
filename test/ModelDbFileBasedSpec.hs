{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module ModelDbFileBasedSpec where

import Test.QuickCheck
import Test.Hspec
import Wallet.ModelDbFileBased
import Control.Monad.State
import Data.Trx
import Data.TrxGen
import MockFileRepo

spec :: Spec 
spec = do 
    describe "ModelDbFileBased" $ do 
        it "is able to read models it wrote" $ do 
            property $ \x -> 
                let res = flip evalState EmptyDir $ runModelDbFileBased "dir/model.dat" $ do 
                        writeModel x
                        x' <- readModel 
                        return $ x == (x' :: Trx)
                in  case (res :: Either ModelDbFileBasedError Bool) of 
                        Right True -> True 
                        _          -> False

        it "throws descriptive error when file that doesn't exist is tried to be read" $
            let res = flip evalState EmptyDir . runModelDbFileBased "dir/model.dat" $ readModel
            in  case (res :: Either ModelDbFileBasedError String) of 
                    Left (ModelFileDoesNotExist "dir/model.dat") -> True
                    _                                            -> False