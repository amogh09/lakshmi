{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Trx
    (
        LakshmiAddress
    ,   Trx (..)
    ,   TrxInput (..)
    ,   TrxOutput (..)
    ,   revenue
    ,   sumOutputValues
    ,   toTrxHashMap
    ,   fromTrxHashMap
    ,   utxos
    ,   splitGroupedKV
    ,   groupInputsByPrevHash
    ,   selectIdxs
    ,   rejectIdxs
    ,   filterUserRevenue
    ,   userUtxos
    ,   sumRevenue
    ,   spendUtxos
    ,   TrxHash
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

type Revenue = (TrxHash, [TrxOutput])

type UTXO = TrxInput

fromTrxHashMap :: TrxHashMap -> [Trx]
fromTrxHashMap = fmap snd . Map.toList

toTrxHashMap :: (Trx -> TrxHash) -> [Trx] -> TrxHashMap
toTrxHashMap trxHashFun ts = Map.fromList $ ts >>= \t -> return (trxHashFun t, t)

revenue :: Set.Set LakshmiAddress -> [Trx] -> [TrxOutput]
revenue as    = foldr f [] where 
    f t os    = let os' = _outputs t
                in  filter myRevenue os' ++ os 
    myRevenue = flip Set.member as . _address

sumOutputValues :: [TrxOutput] -> Integer 
sumOutputValues os = sum (os >>= return . _value)

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

utxoAddress :: TrxHashMap -> UTXO -> LakshmiAddress
utxoAddress m x = _address o where 
    t = m Map.! (_prevTrx x) 
    o = _outputs t !! _index x

filterUserRevenue :: TrxHashMap -> Set.Set LakshmiAddress -> [UTXO] -> [UTXO]
filterUserRevenue m as = filter (flip Set.member as . utxoAddress m)

userUtxos :: TrxHashMap -> Set.Set LakshmiAddress -> TrxHashMap -> [UTXO]
userUtxos m as = filterUserRevenue m as . utxos 

sumRevenue :: [Revenue] -> Integer 
sumRevenue rs = sumOutputValues . join $ [ os | (_,os) <- rs ]

flattenRevenue :: [Revenue] -> [(TrxHash,TrxOutput)]
flattenRevenue rs = join [ [ (h,o) | o <- os ] | (h,os) <- rs ] 

spendUtxos :: Integer -> [Revenue] -> ([(TrxHash,TrxOutput)],[Revenue])
spendUtxos x rs     = (reverse rs', drop (length rs') rs) where 
    rs'             = snd . foldl f (0,[]) . flattenRevenue $ rs 
    f (x',ps) (h,o)
        | x' >= x   = (x',ps)
        | otherwise = (x' + _value o, (h,o):ps)