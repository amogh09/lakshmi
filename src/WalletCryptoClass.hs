module WalletCryptoClass where

import qualified Data.ByteString.UTF8 as BU
import Crypto.Hash
import Data.Bits (shiftL, (.|.))
import Data.List (foldl')
import qualified Data.ByteArray as BA

type LakshmiAddress = Integer

data CryptoError = CryptoError String deriving (Show)

class Monad m => MonadWalletCrytpo m where 
    generateAddress :: String -> m LakshmiAddress

bytesToInteger :: BA.ByteArrayAccess a => a -> Integer 
bytesToInteger = foldl' f 0 . BA.unpack where 
    f x y = (x `shiftL` 8) .|. (fromIntegral y)

strTolakshmiAddress :: String -> LakshmiAddress 
strTolakshmiAddress = tolakshmiAddress . BU.fromString

tolakshmiAddress :: BA.ByteArrayAccess a => a -> LakshmiAddress
tolakshmiAddress x = let dgst  = hashWith SHA256 x
                         dgst' = hashWith RIPEMD160 dgst
                     in  bytesToInteger dgst' 
    