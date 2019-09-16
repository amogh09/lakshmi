module User
    (
        toUserId
    ) where

import CryptoFuns
import WalletCryptoECDSA

type UserId = String 

toUserId :: SeedPhrase -> UserId 
toUserId = hashAndEncodeStr