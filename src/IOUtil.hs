module IOUtil
    (
        catchIO
    ) where

import Control.Monad.IO.Class
import Control.Exception
import Control.Monad.Except
import qualified Data.ByteString as B 

catchIO :: (MonadError e m, Exception ex, MonadIO m) => (ex -> e) -> IO a -> m a
catchIO l r = do 
    e <- liftIO $ (Right <$> r) `catch` (pure . Left . l)
    either throwError pure e