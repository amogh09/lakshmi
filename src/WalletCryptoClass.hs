module WalletCryptoClass where

import qualified Data.ByteString.UTF8 as BU
import Crypto.Hash
import Data.Bits (shiftL, (.|.))
import Data.List (foldl')
import qualified Data.ByteArray as BA

type CryptoAddress = Integer

data CryptoError = CryptoError String deriving (Show)

class Monad m => MonadWalletCrytpo m where 
    generateAddress :: String -> m CryptoAddress

bytesToInteger :: BA.ByteArrayAccess a => a -> Integer 
bytesToInteger = foldl' f 0 . BA.unpack where 
    f x y = (x `shiftL` 8) .|. (fromIntegral y)

strToCryptoAddress :: String -> CryptoAddress 
strToCryptoAddress = toCryptoAddress . BU.fromString

toCryptoAddress :: BA.ByteArrayAccess a => a -> CryptoAddress
toCryptoAddress x = let dgst  = hashWith SHA256 x
                        dgst' = hashWith RIPEMD160 dgst
                    in  bytesToInteger dgst' 
    