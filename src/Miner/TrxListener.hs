module Miner.TrxListener
  (
    startListener
  ) where

import Data.Serialize as S
import Network.Server
import Log.Logger
import Data.Trx
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Network.Types

loggerName :: String 
loggerName = "Miner.TrxListener"

startListener :: TChan Trx -> Port -> IO () 
startListener c port = do 
  updateGlobalLogger loggerName (setLevel DEBUG)
  startServer loggerName port connHandler
  where
    connHandler = singleMsgHandler loggerName (trxHandler c)

trxHandler :: TChan Trx -> MsgHandler
trxHandler c addr bytes = either logErrAndReturn (processTrx c) $ S.decode bytes
  where 
    logErrAndReturn _  = errorM loggerName $ "Failed to decode trx from '" ++ (show bytes) ++ "'"

processTrx :: TChan Trx -> Trx -> IO () 
processTrx c trx = do  
  infoM loggerName $ "Received trx: " ++ show trx
  atomically $ writeTChan c trx