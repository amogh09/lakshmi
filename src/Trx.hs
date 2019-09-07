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
    ,   TrxHash
    ) where

import qualified Data.Serialize as S
import GHC.Generics
import qualified Data.Set as Set
import qualified Data.Map as Map
import ListFuns
import Data.List (sort, groupBy)
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

txis :: [Trx] -> [TrxInput]
txis ts = ts >>= _inputs

groupInputsByPrevHash :: [TrxInput] -> [(TrxHash,[TrxInput])]
groupInputsByPrevHash is = 
    fmap splitGroupedKV . groupBy cmpFst . sort $ [ (_prevTrx i, i) | i <- is ]
    where 
        cmpFst x x' = fst x == fst x'

utxos :: (Trx -> TrxHash) -> [Trx] -> [Revenue] 
utxos thf ts  = 
    filter sndFilled . Map.toList . fmap _outputs . Map.differenceWith f thm $ inputs where     
    sndFilled = not . null . snd
    thm       = toTrxHashMap thf ts
    inputs    = Map.fromList . groupInputsByPrevHash . txis $ ts
    f t is    = case rejectIdxs (fmap _index is) (_outputs t) of 
                    [] -> Nothing 
                    os -> Just $ Trx 0 [] os
    
filterUserRevenue :: Set.Set LakshmiAddress -> [Revenue] -> [Revenue]
filterUserRevenue as = foldr f [] where
    f (h, os) rs = (h,[ o | o <- os, _address o `Set.member` as ]) : rs 

userUtxos :: (Trx -> TrxHash) -> Set.Set LakshmiAddress -> [Trx] -> [Revenue]
userUtxos thf as = filterUserRevenue as . utxos thf 

sumRevenue :: [Revenue] -> Integer 
sumRevenue rs = sumOutputValues . join $ [ os | (_,os) <- rs ]