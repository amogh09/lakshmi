module MonadFileRepoClass 
    ( 
        MonadFileRepo (..)
    ) where

import qualified Data.ByteString as B
import System.Directory (doesFileExist)

class Monad m => MonadFileRepo m where 
    writeBytes :: FilePath -> B.ByteString -> m ()
    readBytes :: FilePath -> m B.ByteString
    fileExists :: FilePath -> m Bool

instance MonadFileRepo IO where 
    writeBytes = B.writeFile
    readBytes = B.readFile
    fileExists = doesFileExist