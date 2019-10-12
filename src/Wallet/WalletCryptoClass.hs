module Wallet.WalletCryptoClass where

import qualified Data.ByteString.UTF8 as BU
import Crypto.Hash
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Trx
import Crypto.CryptoFuns

type CryptoAddress = Integer

data CryptoError = CryptoError String deriving (Show)

class Monad m => MonadWalletCrytpo m where 
    generateAddress :: String -> m CryptoAddress

addresses :: (MonadWalletCrytpo m) => Int -> m [CryptoAddress]
addresses n = mapM generateAddress [ show i | i <- [1..n] ]

trxHash :: (Trx -> BS.ByteString) -> Trx -> Integer
trxHash ser = integerHash . ser

strToCryptoAddress :: String -> CryptoAddress 
strToCryptoAddress = toCryptoAddress . BU.fromString

toCryptoAddress :: BA.ByteArrayAccess a => a -> CryptoAddress
toCryptoAddress x = let dgst  = hashWith SHA256 x
                        dgst' = hashWith RIPEMD160 dgst
                    in  bytesToInteger dgst' 
    