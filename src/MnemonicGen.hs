-- Implements BIP39 mnemonic phase standard

module MnemonicGen where 

import WalletCryptoClass
import Data.Bits (shiftR,complement,shiftL,(.&.))
import qualified Data.ByteArray as BA
import System.Random
import Data.Word
import qualified Data.ByteString as BS 
import Crypto.Hash
import Debug.Trace

randomWord8s :: (RandomGen g) => g -> Int -> ([Word8],g)
randomWord8s g n
    | n <= 0    = ([],g)
    | otherwise = let (w,g')   = random g 
                      (ws,g'') = randomWord8s g' (n-1)
                  in  (w:ws,g'')

seedPhraseIndices :: BitSize -> Entropy -> Either String WordIndices
seedPhraseIndices wlsl2 ent
    | entBytes < 16 = Left $ "Entropy must be at least 16 bytes long - provided " ++ show entBytes ++ " bytes only."
    | r /= 0        = Left $ "Entropy size must be divisible by 4 - provided " ++ show entBytes ++ " bytes."
    | otherwise     = traceShow (fmap toInteger . BS.unpack $ entcs) $ Right $ wordIndices wlsl2 entcs where
    entBytes   = BS.length ent     
    (csBits,r) = entBytes `quotRem` 4
    entDgst    = BA.convert . hashWith SHA256 $ ent
    cs         = takeBits csBits entDgst
    entcs      = ent `BS.append` cs

takeBits :: Int -> BS.ByteString -> BS.ByteString
takeBits n s
    | BS.null s     = BS.empty
    | n <= 0        = BS.empty
    | n <= wordSize = BS.singleton l
    | otherwise     = BS.init p `BS.snoc` l where 
    (q,r) = n `quotRem` wordSize
    p     = BS.take (q+1) s
    lmask = complement $ (1 `shiftL` (wordSize - r)) - 1
    l     = BS.last p .&. lmask    

type Entropy = BS.ByteString
type BitSize = Int
type WordIndices = [Int]

wordSize :: Int
wordSize = 8

wordIndices :: BitSize -> BS.ByteString -> WordIndices
wordIndices wlsl2 n = fmap fromIntegral . reverse . f q . (`shiftR` r) . bytesToInteger $ n where 
        (q,r) = (BS.length n * wordSize) `quotRem` wlsl2
        wls   = 2^wlsl2 
        f 0 _ = [] 
        f i m = (m `mod` wls) : f (i-1) (m `shiftR` wlsl2)