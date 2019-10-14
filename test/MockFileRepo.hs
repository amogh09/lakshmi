{-# LANGUAGE FlexibleInstances #-}

module MockFileRepo
    (
        MockDir (..)
    ) where 

import Control.Exception
import Wallet.MonadFileRepoClass
import Control.Monad.State
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Serialize as S
import qualified Data.ByteString as B

data MockDir = EmptyDir
             | SingleFile FilePath String deriving (Show)

instance Exception [Char]

readStr :: FilePath -> State MockDir String 
readStr p = do 
    dir <- get 
    case dir of 
        EmptyDir -> throw "File does not exist"
        SingleFile p' contents -> 
            if p == p' 
                then pure contents
                else throw "File does not exist" 

instance MonadFileRepo (State MockDir) where
    writeBytes path contents = put (SingleFile path . BU.toString $ contents)    

    fileExists path = do 
        dir <- get 
        case dir of 
            EmptyDir           -> return False 
            SingleFile path' _ -> return $ path == path'

    readString = readStr
        