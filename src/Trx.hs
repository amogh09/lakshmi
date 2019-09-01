{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Trx
    (
        LakshmiAddress
    ,   Trx (..)
    ,   TrxInput (..)
    ,   TrxOutput (..)
    ,   revenue
    ,   sumRevenue
    ,   toTrxHashMap
    ,   fromTrxHashMap
    ,   utxos
    ,   splitGroupedKV
    ,   groupInputsByPrevHash
    ,   selectIdxs
    ,   rejectIdxs
    ) where

import qualified Data.Serialize as S
import GHC.Generics
import qualified Data.Set as Set
import qualified Data.Map as Map
import ListFuns
import Data.List (sort, groupBy)

type LakshmiAddress = String 
type TrxHash = String

data TrxInput = TrxInput {
        _prevTrx :: TrxHash
    ,   _index   :: Int
    } deriving (Show, Generic, S.Serialize, Eq, Ord)

data TrxOutput = TrxOutput {
        _value   :: Integer 
    ,   _address :: LakshmiAddress
    } deriving (Show, Generic, S.Serialize, Eq, Ord)

data Trx = Trx {
        _timestamp :: Integer
    ,   _inputs  :: [TrxInput]
    ,   _outputs :: [TrxOutput]
    } deriving (Show, Generic, S.Serialize, Eq, Ord)

type TrxHashMap = Map.Map TrxHash Trx

fromTrxHashMap :: TrxHashMap -> [Trx]
fromTrxHashMap = fmap snd . Map.toList

toTrxHashMap :: (Trx -> TrxHash) -> [Trx] -> TrxHashMap
toTrxHashMap trxHashFun ts = Map.fromList $ ts >>= \t -> return (trxHashFun t, t)

revenue :: Set.Set LakshmiAddress -> [Trx] -> [TrxOutput]
revenue as    = foldr f [] where 
    f t os    = let os' = _outputs t
                in  filter myRevenue os' ++ os 
    myRevenue = flip Set.member as . _address

sumRevenue :: [TrxOutput] -> Integer 
sumRevenue os = sum (os >>= return . _value)

txis :: [Trx] -> [TrxInput]
txis ts = ts >>= _inputs

groupInputsByPrevHash :: [TrxInput] -> [(TrxHash,[TrxInput])]
groupInputsByPrevHash is = 
    fmap splitGroupedKV . groupBy cmpFst . sort $ [ (_prevTrx i, i) | i <- is ]
    where 
        cmpFst x x' = fst x == fst x'

utxos :: (Trx -> TrxHash) -> [Trx] -> [(TrxHash,[TrxOutput])] 
utxos thf ts = filter (not . null . snd) . Map.toList . Map.intersectionWith f thm $ inputs where 
    thm      = toTrxHashMap thf ts
    inputs   = Map.fromList . groupInputsByPrevHash . txis $ ts
    f t is   = rejectIdxs (fmap _index is) (_outputs t)
    