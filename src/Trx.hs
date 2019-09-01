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

import Type.Reflection        
import qualified Data.Serialize as S
import GHC.Generics
import Control.Exception (Exception)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (groupBy,sort,(\\))

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

filterTrxHashMap :: [Trx] -> TrxHashMap -> TrxHashMap
filterTrxHashMap ts = flip Map.withoutKeys $ Set.fromList $ ts >>= _inputs >>= return . _prevTrx

txos :: [Trx] -> [TrxOutput]
txos ts = ts >>= _outputs

txis :: [Trx] -> [TrxInput]
txis ts = ts >>= _inputs

utxs :: (Trx -> TrxHash) -> [Trx] -> [Trx]
utxs thf ts = fromTrxHashMap . filterTrxHashMap ts . toTrxHashMap thf $ ts

splitGroupedKV :: [(a,b)] -> (a,[b])
splitGroupedKV (x:xs) = (fst x, snd x : fmap snd xs)

selectIdxs :: [Int] -> [a] -> [a]
selectIdxs idxs xs = f (sort idxs) ([0..] `zip` xs) where 
    f [] _          = [] 
    f _ []          = []   
    f (i:is) ((i',x):xs)
        | i == i'   = x : f is xs 
        | otherwise = f (i:is) xs

rejectIdxs :: [Int] -> [a] -> [a]
rejectIdxs idxs xs = selectIdxs ([0..length xs] \\ sort idxs) xs

groupInputsByPrevHash :: [TrxInput] -> [(TrxHash,[TrxInput])]
groupInputsByPrevHash is = 
    fmap splitGroupedKV . groupBy cmpFst . sort $ [ (_prevTrx i, i) | i <- is ]
    where 
        cmpFst x x' = fst x == fst x'

utxos :: (Trx -> TrxHash) -> [Trx] -> [(TrxHash,[TrxOutput])] 
utxos thf ts = filter (not . null . snd) . Map.toList . Map.intersectionWith f thm $ inputs 
    where 
    thm      = toTrxHashMap thf ts
    inputs   = Map.fromList . groupInputsByPrevHash . txis $ ts
    f t is   = rejectIdxs (fmap _index is) (_outputs t)
    