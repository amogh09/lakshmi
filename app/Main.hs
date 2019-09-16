module Main where

import Wallet
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO

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
        command "show" (info showParser (progDesc "Used to display various information about your Wallet. See 'show -h' for details."))
    )

prompt :: String -> IO String 
prompt s = do 
    putStr s 
    hFlush stdout 
    getLine

askSeedPhrase :: IO String 
askSeedPhrase = prompt "Enter your seed-phrase: "

run :: Command -> IO ()
run (Show Balance) = do 
    s <- askSeedPhrase
    let env = mkDefaultEnv s 
    res <- either show show <$> runWallet env checkBalance
    putStrLn res

main :: IO ()
main = run =<< execParser opts where 
    opts = info (parser <**> helper)
        (
            fullDesc
       <>   progDesc "Please use -h option to see usage instructions."
       <>   header   "Welcome to Lakshmi Wallet. Please see usage instructions below."       
        )
