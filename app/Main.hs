module Main where

import Lib
import Options.Applicative
import Data.Semigroup ((<>))

data Command = 
    Show ShowCommand
    deriving (Show)

data ShowCommand = 
        Balance
    |   Address Int
    deriving (Show)

balanceParser :: Parser Command 
balanceParser = pure (Show Balance)

addressParser :: Parser Command 
addressParser = Show <$> Address 
    <$> option auto
        (
            long "count"
       <>   short 'c'
       <>   value 5
       <>   help "number of addresses to be displayed (latest ones first)"
       <>   metavar "<integer>"
        )

showParser :: Parser Command 
showParser = hsubparser
    (
        command "balance" (info balanceParser (progDesc "Show balance"))
   <>   command "address" (info addressParser (progDesc "Show addresses"))
    )

parser :: Parser Command 
parser = hsubparser
    (
        command "show" (info showParser (progDesc "Show info"))
    )

run :: Command -> IO ()
run c = print c

main :: IO ()
main = run =<< execParser opts where 
    opts = info (parser <**> helper)
        (
            fullDesc
       <>   progDesc "Testing this"
        )
