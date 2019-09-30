module Wallet.TupleFuns where

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (x,y) = (f x,y)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (x,y) = (x,f y)

