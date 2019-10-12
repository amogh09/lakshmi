{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wallet.WalletCryptoECDSA
    (
        WalletCryptoECDSA
    ,   runWalletCryptoECDSA
    ,   handleWalletCryptoError
    ,   MonadWalletCrytpo (..)
    ,   WalletCryptoECDSAEnv (..)
    ,   CryptoError
    ,   SeedPhrase
    ) where

import Crypto.CryptoFuns
import Wallet.WalletCryptoClass
import Control.Monad.Reader
import Control.Monad.Except
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.Generate
import qualified Data.ByteString.UTF8 as BU
import Crypto.Hash
import Numeric (showHex)

data WalletCryptoECDSAEnv = WalletCryptoECDSAEnv {
        seedPhrase :: SeedPhrase
    } deriving (Show)

type SeedPhrase = String

curve :: Curve    
curve = getCurveByName SEC_p256k1

hashFun :: SHA256
hashFun = SHA256

newtype WalletCryptoECDSA a = WalletCryptoECDSA { 
        unWalletCryptoDSA :: ExceptT CryptoError (Reader WalletCryptoECDSAEnv) a
    } deriving (Functor, Applicative, Monad, MonadError CryptoError, MonadReader WalletCryptoECDSAEnv)

showPublicPoint :: Point -> String
showPublicPoint (Point x y) = showHex x (":" ++ showHex y "")
showPublicPoint PointO      = show PointO
    
runWalletCryptoECDSA :: WalletCryptoECDSAEnv -> WalletCryptoECDSA a -> Either CryptoError a 
runWalletCryptoECDSA env c = runReader (runExceptT (unWalletCryptoDSA c)) env

handleWalletCryptoError :: CryptoError -> String 
handleWalletCryptoError (CryptoError e) = e

instance MonadWalletCrytpo WalletCryptoECDSA where 
    generateAddress suffix = do 
        env <- ask
        let privKeySeed = hashWith hashFun . BU.fromString $ seedPhrase env ++ suffix
            privKey     = ECDSA.PrivateKey curve . bytesToInteger $ privKeySeed
            pubPoint    = generateQ curve (ECDSA.private_d privKey) 
        pure . strToCryptoAddress . showPublicPoint $ pubPoint
        