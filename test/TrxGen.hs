module TrxGen 
    (
        TrxInput
    ,   TrxOutput
    ,   Trx
    ) where

import Test.QuickCheck
import Trx

instance Arbitrary TrxInput where 
    arbitrary = do 
        ASCIIString p <- arbitrary
        Positive i <- arbitrary
        pure $ TrxInput p i

instance Arbitrary TrxOutput where 
    arbitrary = do 
        Positive v <- arbitrary
        ASCIIString a <- arbitrary
        pure $ TrxOutput v a 

instance Arbitrary Trx where 
    arbitrary = do
        ins  <- arbitrary
        outs <- arbitrary
        pure $ Trx ins outs
