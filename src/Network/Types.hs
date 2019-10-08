module Network.Types 
    (
        Port
    ,   MsgHandler
    ,   ConnHandler
    ) where 

import Network.Socket
import qualified Data.ByteString as BS
import System.IO

type Port = String

type MsgHandler = SockAddr -> BS.ByteString -> IO () 

type ConnHandler = Socket -> SockAddr -> IO ()
