module Log.Logger 
    (
        LoggerName
    ,   setupLogging
    ,   module System.Log.Logger
    ) where 

import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.IO

type LoggerName = String

defaultFormatter :: LogFormatter a
defaultFormatter = simpleLogFormatter "[$time $loggername $prio]> $msg"

setupLogging :: IO ()
setupLogging = do 
    lh <- streamHandler stdout DEBUG
    l  <- getRootLogger    
    let updateFn = setLevel DEBUG . setHandlers [setFormatter lh defaultFormatter]
    updateGlobalLogger rootLoggerName updateFn
