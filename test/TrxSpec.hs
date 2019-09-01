module TrxSpec where 

import Trx
import TrxGen
import Test.Hspec
import Test.QuickCheck
import WalletCryptoClass
import qualified Data.Set as Set
import qualified Data.Serialize as S
import qualified AddressEncoder as A
import Debug.Trace
import Data.List (nub, intersect,(\\))
import Control.Monad

trxHasher = A.encode58 . trxHash S.encode

smallNumber :: Gen Int 
smallNumber = fmap ((`mod` 20) . abs) arbitrary

listOfLen :: (Arbitrary a) => Int -> Gen [a]
listOfLen x = replicateM x arbitrary

listWithElemsLessThan :: Int -> Gen [Int]
listWithElemsLessThan x = sublistOf [0..x-1]

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups :: (Ord a) => [a] -> Bool
hasNoDups = loop Set.empty
    where
    loop _ []       = True
    loop s (x:xs) | s' <- Set.insert x s, Set.size s' > Set.size s
                    = loop s' xs
                    | otherwise
                    = False

spec :: Spec 
spec = do 
    describe "Trx" $ do 
        it "collects user's revenue correctly" $ 
            let o1  = TrxOutput 10 "a"
                o2  = TrxOutput 20 "b"
                o3  = TrxOutput 30 "c"
                ts  = [Trx 1 [] [o1, o2], Trx 2 [] [o3]]
                as  = Set.fromList ["a", "c"]
                res = revenue as ts 
            in  res == [o1, o3]

        it "returns a revenue less than or equal to all output values" $ do
            property $ \(ts,as) -> sumRevenue (revenue as ts) <= sumRevenue (ts >>= _outputs)

        it "converts to and from TrxHashmap correctly" $ do 
            property $ 
                forAll smallNumber $ \x ->
                forAll (listOfLen x) $ \ts ->
                hasNoDups ts ==>
                let out = (fromTrxHashMap . toTrxHashMap trxHasher $ ts) 
                in  ts `intersect` out == ts

    describe "utxos" $ do
        it "returns empty list when no utxos found" $ 
            let (i1,o1)   = (TrxInput "a" 0, TrxOutput 10 "b")
                hashFun _ = "b"
                ts        = [Trx 1 [i1] [o1]]
            in  utxos hashFun ts == []

        it "returns output at correct index" $ 
            let (o1,o1')  = (TrxOutput 10 "a1", TrxOutput 5 "a2")
                t1        = Trx 1 [] [o1,o1']
                t2        = Trx 2 [i2] []
                i2        = TrxInput "h1" 1
                hashFun t
                    | t == t1 = "h1"
                    | t == t2 = "h2"
                ts        = [t1,t2]
                res       = utxos hashFun ts
            in  res == [("h1",[o1])]

        it "finds outputs from multiple trxs" $ 
            let (o1,o1',o1'') = (TrxOutput 10 "o1", TrxOutput 5 "o1'", TrxOutput 20 "ol''")
                t1        = Trx 1 [] [o1,o1',o1'']                                
                i2        = TrxInput "h1" 0                
                o2        = TrxOutput 15 "o2"
                o2'       = TrxOutput 25 "o2'"
                t2        = Trx 2 [i2] [o2,o2']
                i3        = TrxInput "h2" 0
                i3'       = TrxInput "h1" 2
                t3        = Trx 3 [i3,i3'] []
                hashFun t
                    | t == t1 = "h1"
                    | t == t2 = "h2"
                    | t == t3 = "h3"
                res       = utxos hashFun [t1,t2,t3]
            in  res == [("h1",[o1']),("h2",[o2'])]

    describe "selectIdxs" $ do 
        it "selects elements at given ids" $ 
            selectIdxs [5,1,8,15] [0..8] == [1,5,8]

        it "has no element in common with those returned by rejectIdxs" $ 
            property $ \xs -> 
                hasNoDups xs ==>
                forAll (listWithElemsLessThan . length $ xs) $ \is ->
                    let selected = selectIdxs is xs :: [Int]
                        rejected = rejectIdxs is xs 
                    in  selected `intersect` rejected == []
            
    describe "rejectIdxs" $ do 
        it "rejects elements at given ids" $ 
            rejectIdxs [5,1,8,15] [0..18] == ([0..18] \\ [5,1,8,15])

    describe "splitGroupedKV" $ do 
        it "splits key from head and lists all values correctly" $ 
            splitGroupedKV [("a",1),("a",2),("a",3)] == ("a",[1,2,3])

    describe "groupInputsByPrevHash" $ do 
        it "groups inputs by their previous trx hashes correctly" $ 
            let i1  = TrxInput "h1" 0
                i2  = TrxInput "h2" 0
                i1' = TrxInput "h1" 2 
            in  groupInputsByPrevHash [i1,i2,i1'] == [("h1",[i1,i1']), ("h2", [i2])]
