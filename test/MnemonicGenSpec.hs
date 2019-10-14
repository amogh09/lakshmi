module MnemonicGenSpec where

import Wallet.MnemonicGen
import Test.Hspec
import Test.QuickCheck    
import qualified Data.ByteString as BS 
import Wallet.WalletCryptoClass
import qualified Data.Vector as V
import MockFileRepo
import Control.Monad.State
import Crypto.CryptoFuns

type Binary = String 

fromBinary :: Binary -> Integer 
fromBinary = foldl f 0 where 
    f r '1' = r*2 + 1
    f r '0' = r*2    

toBinary :: Integer -> Binary
toBinary = reverse . f where
    f 0  = []
    f x  = let (q,r) = x `quotRem` 2
           in  (if r == 0 then '0' else '1') : f q     

binariesToDecs :: [Binary] -> [Int]
binariesToDecs = fmap (fromIntegral . fromBinary)

toByteString :: [Binary] -> BS.ByteString 
toByteString = BS.pack . fmap fromIntegral . binariesToDecs

spec :: Spec 
spec = do
    describe "fromBinary" $ do 
        it "converts 101 to integer correctly" $ do 
            fromBinary "101" == 5

        it "converts 101010010101010001 to integer correctly" $ do 
            fromBinary "101010010101010001" == 173393

        it "works perfectly with toBinary" $ do 
            property $ \x -> x >= 0 ==> x == fromBinary (toBinary x)

    describe "wordIndices" $ do 
        it "a single index when wordListSizeLog2 is 16 and bits are 32" $
            let bs = BS.pack [10,20]
            in  wordIndices 16 bs == [fromIntegral . bytesToInteger $ bs]

        it "splits input into chunks of bitsize" $ 
            let bs = toByteString ["01011101","10111010","00010010","10011110","00101101"]
            in  wordIndices 11 bs == binariesToDecs ["01011101101","11010000100","10100111100"]

    describe "takeBits" $ do 
        it "extracts bits from the bytestring" $ 
            let bs = toByteString ["11010110","10011011","10101101"]                
            in  takeBits 14 bs == toByteString ["11010110","10011000"]

    describe "loadWordList" $ do 
        it "reads lines from file and converts to a vector of strings" $ 
            let ws  = ["abandon","ability","able","about","above","absent","absorb"]
                p   = "path"
                dir = SingleFile p . unlines $ ws
                r   = evalState (loadWordList p) dir
            in  r == V.fromList ws

    describe "getMnemonicWords" $ do 
        it "extracts words at given indices from a word-list" $ 
            let ws = V.fromList ["abandon","ability","able","about","above","absent","absorb"]
            in  getMnemonicWords ws [0,2,3] == ["abandon","able","about"]

    describe "toMnemonic" $ do 
        it "strings together words to form a seedphrase" $ 
            toMnemonic "-" ["abandon","able","about"] == "abandon-able-about"