{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Data.Block
    (
        Block (..)
    ) where 

import GHC.Generics
import qualified Data.Serialize as S

data Block = Block Int deriving (Show, Eq, Generic, S.Serialize) -- TODO