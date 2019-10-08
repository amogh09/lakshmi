module Data.TChan 
    (
        flushTChan
    ) where 

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

-- Starts a thread that discards all values pushed to the TChan.
-- Useful when the primary channel is used to publish and duplicate channels are 
-- used to receive updates to the channel. This will prevent memory leaks.
flushTChan :: TChan a -> IO ThreadId 
flushTChan c = forkIO . forever $ atomically (readTChan c) >> return ()