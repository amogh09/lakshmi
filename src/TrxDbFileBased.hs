{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TrxDbFileBased
    (
        TrxDbFileBased
    ,   TrxDbFileBasedEnv
    ,   TrxDbError
    ) where

import TrxDbClass
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except
import IOUtil
import System.FilePath ((</>))
import qualified Data.Serialize as S
import qualified Data.ByteString as B

newtype TrxDbFileBased a = TrxDbFileBased {
        unTrxDbFileBased :: ExceptT TrxDbError (ReaderT TrxDbFileBasedEnv IO) a
    } deriving (
        Functor
    ,   Applicative
    ,   Monad
    ,   MonadError TrxDbError
    ,   MonadReader TrxDbFileBasedEnv
    ,   MonadIO
    )
    
data TrxDbFileBasedEnv = TrxDbFileBasedEnv {
        workingDirectory :: FilePath    
    } deriving (Show)

data TrxDbError = TrxDbErrorIO TrxDbFileBasedEnv IOException
                | TrxDbErrorStr TrxDbFileBasedEnv String 
                deriving (Show)

fileName :: TrxDbFileBasedEnv -> FilePath 
fileName env = workingDirectory env </> "trx.dat"

catchTrxIO :: TrxDbFileBasedEnv -> IO a -> TrxDbFileBased a
catchTrxIO env = catchIO (TrxDbErrorIO env) 

runTrxDbFileBased :: TrxDbFileBasedEnv -> TrxDbFileBased a -> IO (Either TrxDbError a)
runTrxDbFileBased e t = runReaderT (runExceptT (unTrxDbFileBased t)) e

instance MonadTrxDb TrxDbFileBased where 
    writeAllTrx ts = do 
        env <- ask 
        catchTrxIO env $ B.writeFile (fileName env) (S.encode ts)

    readAllTrx = do 
        env <- ask 
        bytes <- catchTrxIO env $ B.readFile (fileName env)
        either (throwError . TrxDbErrorStr env) pure . S.decode $ bytes
        