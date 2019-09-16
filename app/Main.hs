module Main where

import Wallet
import UserDbFileBased 
import WalletCryptoECDSA 
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
        command "show" (info showParser (progDesc "Used to display various information about your Wallet. See 'show -h' for details."))
    )

run :: Command -> IO ()
run (Show Balance) = 
    let userDbEnv  = UserDbFileBasedEnv "/tmp/lakshmi" "user"
        cryptoEnv  = WalletCryptoECDSAEnv "passphrase"
        trxDbEnv   = "/tmp/lakshmi/trx.db"
        env        = WalletEnv userDbEnv cryptoEnv trxDbEnv
    in  either show show <$> runWallet env checkBalance >>= putStrLn

main :: IO ()
main = run =<< execParser opts where 
    opts = info (parser <**> helper)
        (
            fullDesc
       <>   progDesc "Please use -h option to see usage instructions."
       <>   header   "Welcome to Lakshmi Wallet. Please see usage instructions below."       
        )
