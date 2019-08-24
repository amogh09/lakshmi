{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UserDbFileBased 
    (
        MonadUserDb (..)
    ,   runUserDbFileBased
    ,   handleUserDbError
    ,   UserDbFileBased
    ,   UserDbError
    ,   UserDbFileBasedEnv (..)
    ) where 

import UserDbClass
import Control.Monad.Reader
import System.FilePath ((</>))
import Control.Monad.Except
import System.IO
import System.IO.Error
import System.Directory
import Control.Exception
import IOUtil

newtype UserDbFileBased a = UserDbFileBased {
        unUserDbFileBased :: ExceptT UserDbError (ReaderT UserDbFileBasedEnv IO) a
    } deriving (
        Functor
    ,   Applicative
    ,   Monad
    ,   MonadError UserDbError
    ,   MonadReader UserDbFileBasedEnv
    ,   MonadIO
    )

data UserDbFileBasedEnv = UserDbFileBasedEnv {
        workingDirectory :: FilePath
    ,   user :: String
    } deriving (Show)

data UserDbError = UserDbErrorIO UserDbFileBasedEnv IOException
                 | UserDbErrorStr UserDbFileBasedEnv String deriving (Show)

runUserDbFileBased :: UserDbFileBasedEnv -> UserDbFileBased a -> IO (Either UserDbError a) 
runUserDbFileBased p u = runReaderT (runExceptT (unUserDbFileBased u)) p

workDir :: UserDbFileBasedEnv -> FilePath
workDir env = workingDirectory env </> user env ++ ".db"

catchUserIO :: UserDbFileBasedEnv -> IO a -> UserDbFileBased a
catchUserIO env = catchIO (UserDbErrorIO env)

handleUserDbError :: UserDbError -> String 
handleUserDbError (UserDbErrorIO env e) 
    | isDoesNotExistErrorType (ioeGetErrorType e) = "Current user '" ++ user env ++ "' has not been registered."
    | otherwise                                   = "Database Error: " ++ (show e)
handleUserDbError (UserDbErrorStr _ s)            = s

instance MonadUserDb UserDbFileBased where 
    getUserSeqNum = do 
        env <- ask
        catchUserIO env $ read <$> withFile (workDir env) ReadMode hGetLine

    setUserSeqNum n = do 
        env <- ask 
        catchUserIO env $ writeFile (workDir env) (show n)

    initUser = do 
        env <- ask 
        let path = workDir env
        exists <- liftIO (doesFileExist path)
        case exists of 
            True -> throwError . UserDbErrorStr env $ "This user has already been registered."
            False -> catchUserIO env $ withFile path WriteMode (`hPutStrLn` "1")