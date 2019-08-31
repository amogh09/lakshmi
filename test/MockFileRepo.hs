{-# LANGUAGE FlexibleInstances #-}

module MockFileRepo
    (
        MockDir (..)
    ) where 

import Control.Exception
import ModelDbFileBased 
import Control.Monad.State
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Serialize as S
import qualified Data.ByteString as B

data MockDir = EmptyDir
             | SingleFile FilePath String deriving (Show)

instance Exception [Char]

instance MonadFileRepo (State MockDir) where
    writeBytes path contents = put (SingleFile path . BU.toString $ contents)

    readBytes path = do 
        dir <- get 
        case dir of 
            EmptyDir -> throw "File does not exist"
            SingleFile path' contents -> 
                if path == path' 
                    then return . BU.fromString $ contents 
                    else throw "File does not exist"

    fileExists path = do 
        dir <- get 
        case dir of 
            EmptyDir           -> return False 
            SingleFile path' _ -> return $ path == path'
