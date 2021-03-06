{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Data.Block where 

import GHC.Generics
import qualified Data.Serialize as S
import qualified Data.ByteString as BS 
import Data.Trx
import Crypto.CryptoFuns

type Nonce = Integer

type Target = Integer

type BlockId = Integer

type BlockHeaderHash = BS.ByteString

newtype Timestamp = Timestamp { unTimestamp :: Integer } deriving (Show, Eq, Ord, S.Serialize, Generic)

newtype MerkleHash = MerkleHash { unMerkleHash :: BS.ByteString } deriving (Show, Eq, Ord, S.Serialize, Generic)

data Block = Block {
        blockHeader :: BlockHeader
    ,   blockTrxs   :: [Trx]
    } deriving (Show, Eq, Generic, S.Serialize)

data BlockHeader = BlockHeader {
        bhId            :: BlockId
    ,   bhPrevBlockHash :: BlockHeaderHash
    ,   bhNonce         :: Nonce 
    ,   bhTarget        :: Target
    ,   bhTimestamp     :: Timestamp 
    ,   bhMerkleHash    :: MerkleHash 
    } deriving (Show, Eq, Generic, S.Serialize)

hashBlock :: BlockHeader -> BlockHeaderHash
hashBlock = hash256 . S.encode