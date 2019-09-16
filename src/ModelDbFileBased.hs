{-# LANGUAGE 
        GeneralizedNewtypeDeriving
    ,   FlexibleInstances
    ,   MultiParamTypeClasses
    ,   UndecidableInstances 
#-}

module ModelDbFileBased 
    (
        readModel
    ,   writeModel
    ,   MonadFileRepo
    ,   readBytes
    ,   writeBytes
    ,   fileExists
    ,   ModelDbFileBased
    ,   runModelDbFileBased
    ,   ModelDbFileBasedError (..)
    ,   liftModelDbError
    ) where

import ModelDbClass
import Control.Monad.Reader
import Control.Monad.Except
import Data.Serialize (encode, decode)
import MonadFileRepoClass

newtype ModelDbFileBased m a = ModelDbFileBased {
        unModelDbFileBased :: ReaderT FilePath (ExceptT ModelDbFileBasedError m) a
    } deriving 
    (
        Functor
    ,   Applicative
    ,   Monad
    ,   MonadReader FilePath
    ,   MonadError ModelDbFileBasedError    
    )

runModelDbFileBased :: FilePath -> ModelDbFileBased m a -> m (Either ModelDbFileBasedError a)
runModelDbFileBased env x = runExceptT (runReaderT (unModelDbFileBased x) env)

data ModelDbFileBasedError = ModelFileDoesNotExist FilePath
                           | ModelDbFileBasedError String
                           deriving (Show)

liftModelDbError :: ModelDbFileBasedError -> String 
liftModelDbError = show

instance MonadTrans ModelDbFileBased where 
    lift = ModelDbFileBased . lift . lift

instance (MonadFileRepo m) => MonadModelDb (ModelDbFileBased m) where 
    writeModel x = do 
        path <- ask    
        let bytes = encode x
        lift $ writeBytes path bytes

    readModel = do 
        path <- ask
        exists <- lift $ fileExists path 
        if exists 
            then do 
                bytes <- lift $ readBytes path 
                case decode bytes of 
                    Left s  -> throwError (ModelDbFileBasedError s)
                    Right x -> return x 
            else throwError . ModelFileDoesNotExist $ path
    