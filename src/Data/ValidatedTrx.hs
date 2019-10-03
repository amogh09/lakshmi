module Data.ValidatedTrx
    (
        ValidatedTrx (..)
    ) where 

import Data.Trx

newtype ValidatedTrx = ValidatedTrx Trx