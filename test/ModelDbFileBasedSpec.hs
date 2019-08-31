{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module ModelDbFileBasedSpec where

import Test.QuickCheck
import Test.Hspec
import ModelDbFileBased
import Control.Monad.State
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Serialize as S
import qualified Data.ByteString as B
import Control.Monad.Catch
import Control.Exception
import Trx
import TrxGen
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
                in  case (res :: Either String Bool) of 
                        Right True -> True 
                        _          -> False