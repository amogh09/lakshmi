{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wallet where

import Control.Monad.Reader
import Control.Monad.Except
import UserDbFileBased
import WalletCryptoECDSA
import AddressEncoder

data WalletEnv = WalletEnv {  
        userDbFileBasedEnv :: UserDbFileBasedEnv
    ,   walletCryptoECDSAEnv :: WalletCryptoECDSAEnv
    } deriving (Show)

data WalletError = WalletUserDbError UserDbError | WalletCryptoError CryptoError
    deriving (Show)

newtype Wallet a = Wallet {
        unWallet :: ExceptT WalletError (ReaderT WalletEnv IO) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader WalletEnv, MonadError WalletError)

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
handleError (WalletUserDbError e) = pure . handleUserDbError $ e 
handleError (WalletCryptoError e) = pure . handleWalletCryptoError $ e

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
