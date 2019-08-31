module ModelDbClass where 

import Data.Serialize

class Monad m => MonadModelDb m where 
    writeModel :: (Serialize a) => a -> m ()
    readModel :: (Serialize a) => m a