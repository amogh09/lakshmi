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

data WalletEnv = WalletEnv {  
        userDbFileBasedEnv :: UserDbFileBasedEnv
    ,   walletCryptoECDSAEnv :: WalletCryptoECDSAEnv
    ,   trxDbFileBasedEnv :: FilePath
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

runWallet :: WalletEnv -> Wallet a -> IO (Either WalletError a) 
runWallet e w = runReaderT (runExceptT (unWallet w)) e

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

registerUser :: Wallet String
registerUser = do 
    liftUserDb initUser 
    pure "User registered successfully."
    `catchError` handleError

liftTrxDbFileBased :: ModelDbFileBased IO a -> Wallet a 
liftTrxDbFileBased d = do 
    p <- asks trxDbFileBasedEnv
    r <- liftIO $ runModelDbFileBased p d
    either (throwError . WalletModelDbError) pure $ r

trxHasher :: Trx -> TrxHash
trxHasher = A.encode58 . trxHash S.encode

-- utxos :: Wallet [Revenue]
-- utxos = do 
--     n <- liftUserDb getUserSeqNum
--     ts <- liftTrxDbFileBased readModel
--     cs <- liftWalletCryptoDSA $ addresses n
--     let as = Set.fromList . fmap A.encode58 $ cs
--     userUtxos trxHasher as $ ts

-- checkBalance :: Wallet Integer 
-- checkBalance = do 
--     rs <- utxos
--     pure . sumRevenue $ rs

-- sendMoney :: [LakshmiAddress,Integer] -> Wallet () 
-- sendMoney os add = do 
--     rs <- utxos 
--     let bal = sumRevenue rs
--         val = sum [ _value o | o <- os ]
--     if bal < val 
--         then throwError $ WalletNotEnoughBalanceError "You don't have enough balance"
--         else let (is,rs') = spendUtxos val rs
--              -- type of UTXO will need to be changed to (TrxHash, Int index, TrxOutput)