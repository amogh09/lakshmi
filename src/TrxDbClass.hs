module TrxDbClass
    (
        MonadTrxDb
    ,   writeAllTrx
    ,   readAllTrx
    ) where 

import Trx

class Monad m => MonadTrxDb m where 
    writeAllTrx :: [Trx] -> m () 
    readAllTrx :: m [Trx]