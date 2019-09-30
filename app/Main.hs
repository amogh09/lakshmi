import Wallet.Wallet
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO
import System.Environment
import Debug.Trace
import Wallet.Trx
import Wallet.ListFuns
import Wallet.TupleFuns
import Log.Logger

data Command = 
        Show ShowCommand
    |   Gen GenCommand
    |   Send [(LakshmiAddress,Integer)]
    deriving (Show)

data ShowCommand = 
        Balance
    |   Address Int
    deriving (Show)

data GenCommand = 
        User 
    |   GenAddress
        deriving (Show)

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
        command "user"    (info genUserParser    (progDesc "Generates new user."))
    <>  command "address" (info genAddressParser (progDesc "Generate a new Lakshmi address. This address is used to receive coins."))
    ) where 
        genUserParser    = pure (Gen User)
        genAddressParser = pure (Gen GenAddress)

sendParser :: Parser Command 
sendParser = Send <$> fmap (mapSnd read . parsePair) <$> some
    (argument str 
        (
            metavar "<pairs>..."              
        <>  help "Address and coins pairs in the format <address>=<integer>"
        )
    )

parser :: Parser Command 
parser = hsubparser
    (
        command "show" (info showParser (progDesc "Command to display various information about your Wallet. See 'show -h' for details."))
    <>  command "gen"  (info genParser  (progDesc "Command to generate various entities."))
    <>  command "send" (info sendParser (progDesc "Send coins to one or more addresses."))
    )

askSeedPhrase :: IO String 
askSeedPhrase = prompt "Enter your seed-phrase: " -- TODO: Use password mode to prevent printing seed-phrase on console

prepareEnv :: IO WalletEnv
prepareEnv = do 
    s    <- askSeedPhrase
    home <- getEnv "HOME"
    pure . mkDefaultEnv home $ s

printErrorOrResult :: Either WalletError String -> IO ()
printErrorOrResult (Right r) = putStrLn r 
printErrorOrResult (Left  e) = putStrLn ("Error: " ++ show e)

run :: Command -> IO ()
run (Show Balance) = do 
    env <- prepareEnv
    runWallet env checkBalance >>= printErrorOrResult
run (Gen User) = do
    s <- genSeedPhrase
    (case s of 
        Left  e -> putStrLn ("Error: " ++ e)
        Right s -> do
            home <- getEnv "HOME"
            let env = mkDefaultEnv home s
            r    <- either show id <$> runWallet env registerUser 
            putStrLn r)
run (Gen GenAddress) = do 
    env <- prepareEnv
    runWallet env newAddress >>= printErrorOrResult
run (Send kvs) = do 
    env  <- prepareEnv
    runWallet env (sendCoins kvs) >>= printErrorOrResult

main :: IO ()
main = do
    setupLogging
    run =<< execParser opts 
    where 
        opts = info (parser <**> helper)
            (
                fullDesc
            <>   progDesc "Please use -h option to see usage instructions."
            <>   header   "Welcome to Lakshmi Wallet. Please see usage instructions below."       
            )
