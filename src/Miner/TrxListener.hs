module Miner.TrxListener
  (
    startListener
  ) where

import Data.Serialize as S
import Miner.Server
import System.Log.Logger
import Wallet.Trx

loggerName :: String 
loggerName = "Lakshmi.TrxListener"

startListener :: String   -- Port
              -> IO () 
startListener port = do 
  updateGlobalLogger loggerName (setLevel DEBUG)
  startServer loggerName port trxHandler 

trxHandler :: MsgHandler
trxHandler addr bytes = either logErrAndReturn handleTrx $ S.decode bytes
  where 
    logErrAndReturn _  = errorM loggerName $ "Failed to decode trx from '" ++ (show bytes) ++ "'"

handleTrx :: Trx -> IO () 
handleTrx = print
