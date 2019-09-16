module UserDbClass where

class Monad m => MonadUserDb m where 
    initUser :: m ()
    getUserSeqNum :: m Int
    setUserSeqNum :: Int -> m ()
    userExists :: m Bool