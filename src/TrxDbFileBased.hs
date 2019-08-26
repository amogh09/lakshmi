{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TrxDbFileBased
    (
        TrxDbFileBased
    ,   TrxDbFileBasedEnv (..)
    ,   runTrxDbFileBased
    ,   TrxDbError
    ,   MonadTrxDbFileBasedIO (..)
    ,   MonadTrxDb (..)
    ) where

import TrxDbClass
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except
import IOUtil
import System.FilePath ((</>))
import qualified Data.Serialize as S
import qualified Data.ByteString as B

newtype TrxDbFileBased f a = TrxDbFileBased {
        unTrxDbFileBased :: ExceptT TrxDbError (ReaderT TrxDbFileBasedEnv f) a
    } deriving (
        Functor
    ,   Applicative
    ,   Monad
    ,   MonadError TrxDbError
    ,   MonadReader TrxDbFileBasedEnv
    ,   MonadIO
    )

instance MonadTrans TrxDbFileBased where 
    lift = TrxDbFileBased . lift . lift

data TrxDbFileBasedEnv = TrxDbFileBasedEnv {
        workingDirectory :: FilePath    
    } deriving (Show)

data TrxDbError = TrxDbErrorIO TrxDbFileBasedEnv IOException
                | TrxDbErrorStr TrxDbFileBasedEnv String 
                deriving (Show)

class Monad f => MonadTrxDbFileBasedIO f where 
    readBytes :: FilePath -> f B.ByteString 
    writeBytes :: FilePath -> B.ByteString -> f () 
    liftTrxDbFileBasedIO :: f a -> TrxDbFileBased f a
    catchTrxDbFileBasedIO :: Exception e => f a -> (e -> f a) -> f a

fileName :: TrxDbFileBasedEnv -> FilePath 
fileName env = workingDirectory env </> "trx.dat"

-- catchTrxIO :: TrxDbFileBasedEnv -> IO a -> TrxDbFileBased a
-- catchTrxIO env = catchIO (TrxDbErrorIO env) 

instance MonadTrxDbFileBasedIO IO where 
    readBytes = B.readFile
    writeBytes = B.writeFile
    liftTrxDbFileBasedIO = liftIO 
    catchTrxDbFileBasedIO = catch

catchTrxIO :: (MonadTrxDbFileBasedIO f) => TrxDbFileBasedEnv -> f a -> TrxDbFileBased f a
catchTrxIO env r = do 
    e <- liftTrxDbFileBasedIO $ (Right <$> r) `catchTrxDbFileBasedIO` (pure . Left . TrxDbErrorIO env)
    either throwError pure e

runTrxDbFileBased :: TrxDbFileBasedEnv -> TrxDbFileBased f a -> f (Either TrxDbError a)
runTrxDbFileBased e t = runReaderT (runExceptT (unTrxDbFileBased t)) e

instance (MonadTrxDbFileBasedIO f) => MonadTrxDb (TrxDbFileBased f) where 
    writeAllTrx ts = do 
        env <- ask 
        catchTrxIO env . writeBytes (fileName env) . S.encode $ ts

    readAllTrx = do 
        env <- ask 
        bytes <- catchTrxIO env . readBytes . fileName $ env
        either (throwError . TrxDbErrorStr env) pure . S.decode $ bytes
        