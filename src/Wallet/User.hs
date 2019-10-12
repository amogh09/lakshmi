module Wallet.User
    (
        toUserId
    ) where

import Crypto.CryptoFuns
import Wallet.WalletCryptoECDSA

type UserId = String 

toUserId :: SeedPhrase -> UserId 
toUserId = hashAndEncodeStr