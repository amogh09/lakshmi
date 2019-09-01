module ListFuns 
    (
        selectIdxs
    ,   rejectIdxs
    ,   splitGroupedKV
    ,   hasNoDups
    ) where 

import qualified Data.Set as Set
import Data.List
import Control.Exception

selectIdxs :: [Int] -> [a] -> [a]
selectIdxs idxs ys = f (sort idxs) ([0..] `zip` ys) where 
    f [] _          = [] 
    f _ []          = []   
    f (i:is) ((i',x):xs)
        | i == i'   = x : f is xs 
        | otherwise = f (i:is) xs
        
rejectIdxs :: [Int] -> [a] -> [a]
rejectIdxs idxs xs = selectIdxs ([0..length xs] \\ sort idxs) xs

newtype InvalidArgumentException = InvalidArgumentException String 
    deriving (Show)

instance Exception InvalidArgumentException

splitGroupedKV :: [(a,b)] -> (a,[b])
splitGroupedKV (x:xs) = (fst x, snd x : fmap snd xs)
splitGroupedKV []     = throw $ InvalidArgumentException "Input to splitGroupedKV cannot be empty" 

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups :: (Ord a) => [a] -> Bool
hasNoDups = loop Set.empty
    where
    loop _ []       = True
    loop s (x:xs) | s' <- Set.insert x s, Set.size s' > Set.size s
                    = loop s' xs
                    | otherwise
                    = False