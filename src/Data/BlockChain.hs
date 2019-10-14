module Data.BlockChain 
    (
        BlockChain
    ,   bcHead
    ,   putBlock
    ,   empty
    ,   BlockChains
    ) where 

import Data.List 
import Data.ListFuns
import Data.Block
import Data.Maybe
import qualified Data.Map as M 
import qualified Data.ByteString as BS 
import Control.Monad.State
import Control.Monad

data ChainBlock = ChainBlock {
        cbBlock     :: Block 
    ,   cbBlockHash :: BlockHeaderHash
    } deriving (Show, Eq)

type BlockChain = [ChainBlock]

type BlockChains = [BlockChain]

bcHead :: BlockChains -> Block 
bcHead = cbBlock . head . maximumOn (bhId . blockHeader . cbBlock . head)

findBlockChain :: BlockHeaderHash -> BlockChain -> State (M.Map BlockHeaderHash (Maybe BlockChain)) (Maybe BlockChain)
findBlockChain h []      = return Nothing
findBlockChain h (cb:bc) = do 
    cache <- get 
    if h `M.member` cache 
        then return $ cache M.! h
    else if cbBlockHash cb == h 
        then return . Just $ bc
        else do 
             r <- findBlockChain h bc 
             let cache' = M.insert (cbBlockHash cb) r cache 
             put cache'
             return r  

putBlock :: Block -> BlockChains -> Either String BlockChains
putBlock b bcs = 
    case find ((== bhPrevBlockHash (blockHeader b)) . cbBlockHash . head) bcs of 
        Just bc -> Right . ((cb:bc):) . deleteOn ((== cbBlockHash (head bc)) . cbBlockHash . head) $ bcs
        Nothing -> 
            case find isJust $ execState (mapM (findBlockChain . bhPrevBlockHash . blockHeader $ b) bcs) M.empty of 
                Just (Just bc) -> Right $ (cb:bc) : bcs
                _              -> Left "Previous block not found"
    where cb = ChainBlock b (hashBlock . blockHeader $ b)

empty :: BlockChains
empty = []