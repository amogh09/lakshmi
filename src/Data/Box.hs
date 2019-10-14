module Data.Box 
    (
        BlockMakerReadBox (..)
    ,   BCMReadChan (..)
    ,   BPReadChan (..)
    ) where

import Data.Block
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar

newtype BlockMakerReadBox  = BlockMakerReadBox  { unBlockMakerReadBox  :: TMVar BlockHeader }

newtype BCMReadChan = BCMReadChan { unBCMReadChan :: TChan Block }
        
newtype BPReadChan = BPReadChan { unBPReadChan :: TChan Block }