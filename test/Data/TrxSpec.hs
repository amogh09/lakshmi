module Data.TrxSpec where 

import Data.Trx
import Data.TrxGen
import Test.Hspec
import Test.QuickCheck
import Wallet.WalletCryptoClass
import qualified Data.Set as Set
import qualified Data.Serialize as S
import qualified Wallet.AddressEncoder as A
import Data.List (nub, intersect,(\\))
import Control.Monad
import TestFuns
import Data.ListFuns

trxHasher = A.encode58 . trxHash S.encode

spec :: Spec 
spec = do 
    describe "fromTrxHashMap" $ do
        it "returns the input to toTrxHashmap" $ do 
            property $ 
                forAll smallNumber $ \x ->
                forAll (listOfLen x) $ \ts ->
                hasNoDups ts ==>
                let out = (fromTrxHashMap . toTrxHashMap trxHasher $ ts) 
                in  ts `intersect` out == ts

    describe "utxos" $ do
        it "returns empty list when no utxos found" $ 
            let hashFun _ = "b"
                ts        = [Trx 1 [TrxInput "a" 0] []]
                m         = toTrxHashMap hashFun ts
                res       = utxos m 
            in  res == []

        it "returns output at correct index" $ 
            let (o1,o1',o1'') = (TrxOutput 10 "a1", TrxOutput 5 "a2", TrxOutput 5 "a3")
                t1       = Trx 1 [] [o1,o1',o1'']
                i2       = TrxInput "h1" 1
                t2       = Trx 2 [i2] []
                hashFun t
                    | t == t1 = "h1"
                    | t == t2 = "h2"
                ts       = [t1,t2]
                m        = toTrxHashMap hashFun ts
            in  utxos m == [TrxInput "h1" 0, TrxInput "h1" 2]

        it "finds outputs from multiple trxs" $ 
            let (o1,o1',o1'') = (TrxOutput 10 "o1", TrxOutput 5 "o1'", TrxOutput 20 "ol''")
                t1  = Trx 1 [] [o1,o1',o1'']                                
                i2  = TrxInput "h1" 0                
                o2  = TrxOutput 15 "o2"
                o2' = TrxOutput 25 "o2'"
                t2  = Trx 2 [i2] [o2,o2']
                i3  = TrxInput "h2" 0
                i3' = TrxInput "h1" 2
                t3  = Trx 3 [i3,i3'] []
                hashFun t
                    | t == t1 = "h1"
                    | t == t2 = "h2"
                    | t == t3 = "h3"
                m   = toTrxHashMap hashFun [t1,t2,t3]
            in  utxos m == [TrxInput "h1" 1, TrxInput "h2" 1]

        it "plays well with genesis transaction" $ 
            let o1 = TrxOutput 100 "a"
                t1 = Trx 0 [] [o1]
                hashFun _ = "h1"
                m  = toTrxHashMap hashFun [t1]
            in  utxos m == [TrxInput "h1" 0]

    describe "groupInputsByPrevHash" $ do 
        it "groups inputs by their previous trx hashes correctly" $ 
            let i1  = TrxInput "h1" 0
                i2  = TrxInput "h2" 0
                i1' = TrxInput "h1" 2 
            in  groupInputsByPrevHash [i1,i2,i1'] == [("h1",[i1,i1']), ("h2", [i2])]

    describe "filterUserUtxos" $ do 
        it "selects user's revenue correctly" $ 
            let o1 = TrxOutput 10 "o1"
                o2 = TrxOutput 20 "o2"
                o3 = TrxOutput 30 "o3"
                o4 = TrxOutput 40 "o4"
                t1 = Trx 0 [] [o1,o2]
                t2 = Trx 0 [] [o3,o4]
                hashFun t 
                    | t == t1 = "1"
                    | t == t2 = "2"
                m  = toTrxHashMap hashFun [t1,t2]
                xs = utxos m
                as = Set.fromList ["o2","o3"]
            in  filterUserUtxos m as xs == [TrxInput "1" 1,TrxInput "2" 0]

    describe "sumUtxos" $ do 
        it "sums values of outputs corresponding to UTXOs" $ 
            let o1 = TrxOutput 10 "o1"
                o2 = TrxOutput 20 "o2"
                o3 = TrxOutput 30 "o3"
                o4 = TrxOutput 40 "o4"
                t1 = Trx 0 [] [o1,o2]
                t2 = Trx 0 [] [o3,o4]
                hashFun t 
                    | t == t1 = "1"
                    | t == t2 = "2"
                m  = toTrxHashMap hashFun [t1,t2]
                xs = utxos m 
            in  sumUtxos m xs == 10 + 20 + 30 + 40
