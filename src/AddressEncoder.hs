module AddressEncoder (encode58) where 

import qualified Data.Vector as V

type EncoderDict = V.Vector Char

encode :: EncoderDict -> Integer -> String 
encode chars x 
    | x < base  = [chars V.! (fromIntegral x)]
    | otherwise = let i = x `mod` base 
                  in  chars V.! (fromIntegral i) : encode chars (x `quot` base) where 
        base = toInteger . V.length $ chars

chars58 :: EncoderDict
chars58 = V.fromList $ "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHJKMNPQRSTUVWXYZ"

encode58 :: Integer -> String 
encode58 = encode chars58
