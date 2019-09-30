module Wallet.User
    (
        toUserId
    ) where

import Wallet.CryptoFuns
import Wallet.WalletCryptoECDSA

type UserId = String 

toUserId :: SeedPhrase -> UserId 
toUserId = hashAndEncodeStr