{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Trx
    (
        LakshmiAddress
    ,   Trx (..)
    ,   TrxInput (..)
    ,   TrxOutput (..)    
    ,   toTrxHashMap
    ,   fromTrxHashMap
    ,   utxos
    ,   splitGroupedKV
    ,   groupInputsByPrevHash
    ,   selectIdxs
    ,   rejectIdxs
    ,   filterUserUtxos
    ,   userUtxos
    ,   spendUtxos
    ,   TrxHash
    ,   sumUtxos
    ) where

import qualified Data.Serialize as S
import GHC.Generics
import qualified Data.Set as Set
import qualified Data.Map as Map
import ListFuns
import Data.List (sort, groupBy, (\\))
import Control.Monad

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

type UTXO = TrxInput

fromTrxHashMap :: TrxHashMap -> [Trx]
fromTrxHashMap = fmap snd . Map.toList

toTrxHashMap :: (Trx -> TrxHash) -> [Trx] -> TrxHashMap
toTrxHashMap trxHashFun ts = Map.fromList $ ts >>= \t -> return (trxHashFun t, t)

groupInputsByPrevHash :: [TrxInput] -> [(TrxHash,[TrxInput])]
groupInputsByPrevHash is = 
    fmap splitGroupedKV . groupBy cmpFst . sort $ [ (_prevTrx i, i) | i <- is ]
    where 
        cmpFst x x' = fst x == fst x'

txis :: TrxHashMap -> [TrxInput]
txis m = Map.toList m >>= \(_,t) -> _inputs t

utxos :: TrxHashMap -> [UTXO]
utxos m     = fmap (uncurry TrxInput) (ls' ++ rs') where 
    ls       = m `Map.difference` inputMap 
    rs       = Map.intersectionWith f m inputMap 
    inputMap = fmap (fmap _index) . Map.fromList . groupInputsByPrevHash . txis $ m
    f t      = (\\) [0..length (_outputs t) - 1]
    ls'      = flattenKeyVal . Map.toList . fmap (\t -> [0..length (_outputs t)-1]) $ ls 
    rs'      = flattenKeyVal . Map.toList $ rs

utxoData :: TrxHashMap -> UTXO -> TrxOutput
utxoData m x = _outputs t !! _index x where 
    t = m Map.! (_prevTrx x) 

filterUserUtxos :: TrxHashMap -> Set.Set LakshmiAddress -> [UTXO] -> [UTXO]
filterUserUtxos m as = filter (flip Set.member as . _address . utxoData m)

userUtxos :: TrxHashMap -> Set.Set LakshmiAddress -> TrxHashMap -> [UTXO]
userUtxos m as = filterUserUtxos m as . utxos 

spendUtxos :: TrxHashMap -> Integer -> [UTXO] -> ([UTXO],[UTXO])
spendUtxos m v [] = ([],[])
spendUtxos m v (x:xs)
    | v <= 0      = ([],x:xs)
    | otherwise   = 
        let (ls,rs) = spendUtxos m (v-vi) xs
            vi      = _value . flip (!!) (_index x) . _outputs . (Map.!) m . _prevTrx $ x
        in  (x:ls,rs)

sumUtxos :: TrxHashMap -> [UTXO] -> Integer 
sumUtxos m = sum . fmap _value . fmap (utxoData m)