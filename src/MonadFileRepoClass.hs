module MonadFileRepoClass 
    ( 
        MonadFileRepo (..)
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import System.Directory (doesFileExist)
import System.IO (readFile)

class Monad m => MonadFileRepo m where 
    writeBytes :: FilePath -> BS.ByteString -> m ()

    readBytes :: FilePath -> m BS.ByteString
    readBytes p = readString p >>= pure . BU.fromString

    fileExists :: FilePath -> m Bool

    readString :: FilePath -> m String

instance MonadFileRepo IO where 
    writeBytes = BS.writeFile
    readBytes = BS.readFile
    fileExists = doesFileExist
    readString = readFile