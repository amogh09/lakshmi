module Main where

import Wallet
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO
import System.Environment
import Debug.Trace

data Command = 
        Show ShowCommand
    |   Gen GenCommand
    deriving (Show)

data ShowCommand = 
        Balance
    |   Address Int
    deriving (Show)

data GenCommand = User deriving (Show)

showParser :: Parser Command 
showParser = hsubparser
    (
        command "balance" (info balanceParser (progDesc "Show balance"))
   <>   command "address" (info addressParser (progDesc "Show addresses"))
    ) where 
        balanceParser = pure (Show Balance)
        addressParser = Show <$> Address <$> 
            option auto
                (
                    long "count"
               <>   short 'c'
               <>   value 5
               <>   help "number of addresses to be displayed (latest ones first)"
               <>   metavar "<integer>"
                )
    
genParser :: Parser Command 
genParser = hsubparser
    (
        command "user" (info genUserParser (progDesc "Generates new user."))
    ) where 
        genUserParser = pure (Gen User)
            
parser :: Parser Command 
parser = hsubparser
    (
        command "show" (info showParser (progDesc "Command to display various information about your Wallet. See 'show -h' for details."))
   <>   command "gen"  (info genParser  (progDesc "Command to generate various entities."))
    )

askSeedPhrase :: IO String 
askSeedPhrase = prompt "Enter your seed-phrase: "

run :: Command -> IO ()
run (Show Balance) = do 
    s    <- askSeedPhrase
    home <- getEnv "HOME"
    let env = mkDefaultEnv home s 
    res  <- either show id <$> runWallet env checkBalance
    putStrLn res
run (Gen User) = do
    s <- genSeedPhrase
    (case s of 
        Left  e -> putStrLn ("Error: " ++ e)
        Right s -> do
            home <- getEnv "HOME"
            let env = mkDefaultEnv home s
            r    <- either show id <$> runWallet env registerUser 
            putStrLn r)

main :: IO ()
main = run =<< execParser opts where 
    opts = info (parser <**> helper)
        (
            fullDesc
       <>   progDesc "Please use -h option to see usage instructions."
       <>   header   "Welcome to Lakshmi Wallet. Please see usage instructions below."       
        )
