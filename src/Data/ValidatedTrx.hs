module Data.ValidatedTrx
    (
        ValidatedTrx 
    ,   toTrx
    ,   fromTrx
    ) where 

import Data.Trx

newtype ValidatedTrx = ValidatedTrx { unValidatedTrx :: Trx }

fromTrx :: Trx -> ValidatedTrx
fromTrx = ValidatedTrx

toTrx :: ValidatedTrx -> Trx 
toTrx = unValidatedTrx