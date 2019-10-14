module Data.ListFunsSpec where 

import Test.Hspec
import Test.QuickCheck    
import Data.ListFuns
import Data.List ((\\), intersect)
import TestFuns

spec :: Spec 
spec = do
    describe "selectIdxs" $ do 
        it "selects elements at given indexes" $ 
            selectIdxs [5,1,8,15] [0..8] == [1,5,8]

        it "has no element in common with those returned by rejectIdxs" $ 
            property $ \xs -> 
                hasNoDups xs ==>
                forAll (listWithElemsLessThan . length $ xs) $ \is ->
                    let selected = selectIdxs is xs :: [Int]
                        rejected = rejectIdxs is xs 
                    in  selected `intersect` rejected == []

        it "selects nothing if indexes are all out of bounds" $ 
            property $ \xs ->
                forAll (listWithElemsAtLeast . length $ (xs :: [Int])) $ \is -> 
                    null . selectIdxs is $ xs
                    
    describe "rejectIdxs" $ do 
        it "rejects elements at given ids" $ 
            rejectIdxs [5,1,8,15] [0..18] == ([0..18] \\ [5,1,8,15])

        it "selects everything if indexes are all out of bounds" $ 
            property $ \xs ->
                forAll (listWithElemsAtLeast . length $ (xs :: [Int])) $ \is -> 
                    rejectIdxs is xs == xs
    
    describe "splitGroupedKV" $ do 
        it "splits key from head and lists all values correctly" $ 
            splitGroupedKV [("a",1),("a",2),("a",3)] == ("a",[1,2,3])
