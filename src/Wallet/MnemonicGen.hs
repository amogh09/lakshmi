-- Implements BIP39 mnemonic phase standard

module Wallet.MnemonicGen
    (
        getSeedPhrase
    ,   loadWordList
    ,   genEntropy 
    ,   toMnemonic
    ,   getMnemonicWords
    ,   takeBits
    ,   wordIndices
    ) where 

import Data.Bits (shiftR,complement,shiftL,(.&.))
import qualified Data.ByteArray as BA
import System.Random
import Data.Word
import qualified Data.ByteString as BS 
import Crypto.Hash
import qualified Data.Vector as V
import Wallet.MonadFileRepoClass
import Data.List (intercalate)
import Crypto.CryptoFuns

randomWord8s :: (RandomGen g) => g -> Int -> ([Word8],g)
randomWord8s g n
    | n <= 0    = ([],g)
    | otherwise = let (w,g')   = random g 
                      (ws,g'') = randomWord8s g' (n-1)
                  in  (w:ws,g'')

genEntropy :: IO Entropy
genEntropy = do 
    g      <- newStdGen
    let (xs,_) = randomWord8s g 16
    return . BS.pack $ xs

getSeedPhrase :: WordList -> Extension -> Entropy -> Either String Mnemonic 
getSeedPhrase wl ext ent = do 
    is <- getSeedPhraseIndices 11 ent
    let m = (ext:) . getMnemonicWords wl $ is 
    pure $ toMnemonic getSeedPhraseDelim m

getSeedPhraseIndices :: BitSize -> Entropy -> Either String WordIndices
getSeedPhraseIndices wlsl2 ent
    | entBytes < 16 = Left $ "Entropy must be at least 16 bytes long - provided " ++ show entBytes ++ " bytes only."
    | r /= 0        = Left $ "Entropy size must be divisible by 4 - provided " ++ show entBytes ++ " bytes."
    | otherwise     = Right $ wordIndices wlsl2 entcs where
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
type WordList = V.Vector String
type Words = [String]
type Mnemonic = String
type Extension = String
type Delimiter = String

wordSize :: Int
wordSize = 8

wordIndices :: BitSize -> BS.ByteString -> WordIndices
wordIndices wlsl2 n = fmap fromIntegral . reverse . f q . (`shiftR` r) . bytesToInteger $ n where 
        (q,r) = (BS.length n * wordSize) `quotRem` wlsl2
        wls   = 2^wlsl2 
        f 0 _ = [] 
        f i m = (m `mod` wls) : f (i-1) (m `shiftR` wlsl2)

getMnemonicWords :: WordList -> WordIndices -> Words
getMnemonicWords ws = fmap (ws V.!)
 
getSeedPhraseDelim :: Delimiter
getSeedPhraseDelim = "-"

toMnemonic :: Delimiter -> Words -> Mnemonic
toMnemonic = intercalate

loadWordList :: (MonadFileRepo m) => FilePath -> m WordList 
loadWordList p = readString p >>= pure . lines >>= pure . V.fromList