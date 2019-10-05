module Data.BlockChain 
    (
        BlockChain
    ,   bcHead
    ,   putBlock
    ,   empty
    ) where 

import Data.Block

data BlockChain = BlockChain { unBlockChain :: [Block] } deriving (Show) -- TODO Proper data structure

bcHead :: BlockChain -> Block 
bcHead (BlockChain bc) = Prelude.head bc

putBlock :: Block -> BlockChain -> BlockChain
putBlock b = BlockChain . (b:) . unBlockChain 

empty :: BlockChain 
empty = BlockChain [] 