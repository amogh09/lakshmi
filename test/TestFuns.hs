module TestFuns 
    (
        smallNumber
    ,   listOfLen
    ,   listWithElemsLessThan
    ,   listWithElemsAtLeast
    ) where 

import Test.QuickCheck
import Control.Monad

smallNumber :: Gen Int 
smallNumber = fmap ((`mod` 20) . abs) arbitrary

listOfLen :: (Arbitrary a) => Int -> Gen [a]
listOfLen x = replicateM x arbitrary

listWithElemsLessThan :: Int -> Gen [Int]
listWithElemsLessThan x = sublistOf [0..x-1]

listWithElemsAtLeast :: Int -> Gen [Int]
listWithElemsAtLeast x = sublistOf [x..x+100]