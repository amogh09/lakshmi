module Wallet.TrxDbClass
    (
        MonadTrxDb
    ,   writeAllTrx
    ,   readAllTrx
    ) where 

import Wallet.Trx

class Monad m => MonadTrxDb m where 
    writeAllTrx :: [Trx] -> m () 
    readAllTrx :: m [Trx]