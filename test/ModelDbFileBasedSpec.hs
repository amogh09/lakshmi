{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module ModelDbFileBasedSpec where

import Test.QuickCheck
import Test.Hspec
import ModelDbFileBased
import Control.Monad.State
import Trx
import TrxGen
import MockFileRepo
import Debug.Trace

spec :: Spec 
spec = do 
    describe "ModelDbFileBased" $ do 
        it "is able to read models it wrote" $ do 
            property $ \x -> 
                let res = flip evalState EmptyDir $ runModelDbFileBased "dir/model.dat" $ do 
                        writeModel x
                        x' <- readModel 
                        return $ x == (x' :: Trx)
                in  case (res :: Either String Bool) of 
                        Right True -> True 
                        _          -> False

        it "throws descriptive error when file that doesn't exist is tried to be read" $
            let res = flip evalState EmptyDir . runModelDbFileBased "dir/model.dat" $ readModel
            in  case (res :: Either String String) of 
                    Left "File dir/model.dat does not exist." -> True                     
                    _                                         -> False