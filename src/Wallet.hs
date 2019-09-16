{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wallet where

import Control.Monad.Reader
import Control.Monad.Except
import UserDbFileBased
import WalletCryptoECDSA
import AddressEncoder
import ModelDbFileBased
import qualified Data.Serialize as S
import qualified AddressEncoder as A
import Trx
import WalletCryptoClass
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import User
import System.IO
import MnemonicGen

data WalletEnv = WalletEnv {  
        workDir              :: FilePath
    ,   userDbFileBasedEnv   :: UserDbFileBasedEnv
    ,   walletCryptoECDSAEnv :: WalletCryptoECDSAEnv
    ,   trxDbFileBasedEnv    :: FilePath
    } deriving (Show)

data WalletError = WalletUserDbError UserDbError 
                 | WalletCryptoError CryptoError
                 | WalletModelDbError String
                 | WalletNotEnoughBalanceError String 
    deriving (Show)

newtype Wallet a = Wallet {
        unWallet :: ExceptT WalletError (ReaderT WalletEnv IO) a
    } deriving 
    (
        Functor
    ,   Applicative
    ,   Monad
    ,   MonadIO
    ,   MonadReader WalletEnv
    ,   MonadError WalletError
    )

mkDefaultEnv :: FilePath -> SeedPhrase -> WalletEnv 
mkDefaultEnv home s = WalletEnv wd userDbEnv cryptoEnv trxEnv where 
    wd         = home </> ".lakshmi"
    userId     = toUserId s
    userDbEnv  = UserDbFileBasedEnv wd userId
    cryptoEnv  = WalletCryptoECDSAEnv s 
    trxEnv     = wd </> "trx.db"

runWallet :: WalletEnv -> Wallet a -> IO (Either WalletError a) 
runWallet e w = do
    createDirectoryIfMissing True (workDir e)
    runReaderT (runExceptT (unWallet w)) e

liftUserDb :: UserDbFileBased a -> Wallet a 
liftUserDb u = do 
    p <- asks userDbFileBasedEnv
    join . liftIO $ fmap (either (throwError . WalletUserDbError) pure) (runUserDbFileBased p u)

liftWalletCryptoDSA :: WalletCryptoECDSA a -> Wallet a 
liftWalletCryptoDSA w = do 
    s <- asks walletCryptoECDSAEnv
    either (throwError . WalletCryptoError) pure $ runWalletCryptoECDSA s w

handleError :: WalletError -> Wallet String 
handleError (WalletUserDbError e)           = pure . handleUserDbError $ e 
handleError (WalletCryptoError e)           = pure . handleWalletCryptoError $ e
handleError (WalletModelDbError s)          = pure s
handleError (WalletNotEnoughBalanceError s) = pure s

newAddress :: Wallet String
newAddress = do 
    n <- liftUserDb getUserSeqNum
    addr <- liftWalletCryptoDSA . generateAddress . show $ n
    liftUserDb . setUserSeqNum $ n + 1
    pure . encode58 $ addr
    `catchError` handleError

prompt :: String -> IO String 
prompt s = do 
    putStr s 
    hFlush stdout 
    getLine

genSeedPhrase :: IO (Either String String)
genSeedPhrase = do 
    ext      <- prompt "Enter an extension to the seed-phrase: "
    wordList <- loadWordList "wordlist.txt"
    ent      <- genEntropy
    pure $ getSeedPhrase wordList ext ent    

registerUser :: Wallet String
registerUser = do 
    liftUserDb initUser 
    e <- ask 
    let s = seedPhrase . walletCryptoECDSAEnv $ e
    pure $ "User registered successfully.\nSeed phrase is: " ++ s
    `catchError` handleError

liftTrxDbFileBased :: ModelDbFileBased IO a -> Wallet a 
liftTrxDbFileBased d = do 
    p <- asks trxDbFileBasedEnv
    r <- liftIO $ runModelDbFileBased p d
    either (throwError . WalletModelDbError . liftModelDbError) pure $ r

trxHasher :: Trx -> TrxHash
trxHasher = A.encode58 . trxHash S.encode

trxs :: Wallet TrxHashMap
trxs = liftTrxDbFileBased $ do 
    ts <- readModel `catchError` cf 
    pure . toTrxHashMap trxHasher $ ts
    where 
        cf (ModelFileDoesNotExist _) = pure []

getUtxos :: TrxHashMap -> Wallet [UTXO]
getUtxos m = do 
    n  <- liftUserDb getUserSeqNum
    cs <- liftWalletCryptoDSA $ addresses n
    let as = Set.fromList . fmap A.encode58 $ cs
    pure . userUtxos m as $ m

checkBalance :: Wallet String 
checkBalance = do 
    m  <- trxs
    xs <- getUtxos m
    pure . show . sumUtxos m $ xs
    `catchError` handleError

pushTrx :: TrxHashMap -> Trx -> Wallet ()
pushTrx m t = let m' = Map.insert (trxHasher t) t m
              in  liftTrxDbFileBased . writeModel . fromTrxHashMap $ m'

generateTrx :: TrxHashMap -> [(LakshmiAddress,Integer)] -> Wallet Trx
generateTrx m ys = do 
    xs <- getUtxos m
    let bal = sumUtxos m xs
        os  = fmap (uncurry . flip $ TrxOutput) ys
        val = sum . fmap _value $ os
    if bal < val 
        then throwError $ WalletNotEnoughBalanceError "You don't have enough balance"
        else let (ls,_) = spendUtxos m val xs
                 change  = sumUtxos m ls - val
             in  if change > 0 
                    then do 
                        c <- newAddress
                        let co = TrxOutput change c
                        pure  . Trx 0 ls $ co:os
                    else pure . Trx 0 ls $ os

sendCoins :: [(LakshmiAddress,Integer)] -> Wallet String
sendCoins ys = do
    m <- trxs
    t <- generateTrx m ys 
    pushTrx m t
    pure "Transaction published successfully."
    `catchError` handleError