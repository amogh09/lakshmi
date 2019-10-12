-- Module containing helpful crypto functions

module Crypto.CryptoFuns where

import Wallet.AddressEncoder
import Crypto.Hash
import Data.Bits (shiftL, (.|.),shiftR)
import Data.List (foldl')
import qualified Data.ByteString as BS 
import qualified Data.ByteArray as BA
import qualified Data.ByteString.UTF8 as BU

hashAndEncodeStr :: String -> String 
hashAndEncodeStr = hashAndEncode . BU.fromString

hashAndEncode :: BS.ByteString -> String
hashAndEncode = encode58 . integerHash

integerHash :: BS.ByteString -> Integer
integerHash = bytesToInteger . hash256

hash256 :: BS.ByteString -> BS.ByteString
hash256 = BA.convert . hashWith SHA256

bytesToInteger :: BA.ByteArrayAccess a => a -> Integer 
bytesToInteger = foldl' f 0 . BA.unpack where 
    f x y = (x `shiftL` 8) .|. (fromIntegral y)

integerToBytes :: Integer -> BS.ByteString
integerToBytes = BS.reverse . f where
    f 0 = BS.empty 
    f n = fromIntegral (n `mod` 256) `BS.cons` f (n `shiftR` 8)

merkleHash :: [BS.ByteString] -> BS.ByteString 
merkleHash [x] = x 
merkleHash xs  = merkleHash . pairs $ xs where 
    pairs (y:y':ys) = hash256 (y `BS.append` y') : pairs ys 
    pairs [y]       = pairs [y,y]
    pairs []        = []