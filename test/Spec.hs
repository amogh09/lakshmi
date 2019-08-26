import TrxDbFileBased
import TestTrxDbFileBased
import Test.Hspec
import Test.QuickCheck
import Control.Monad.State
import qualified Data.ByteString as B
import TrxDbFileBased
import Data.Either

main :: IO ()
main = hspec $ do 
    describe "TrxDbFileBased.writeAllTrx" $ do 
        it "writes all transactions to file after serialization" $ do 
            property $ \ts -> 
                isRight . evalMockFileBasedIO (SingleFile "" "") . runTrxDbFileBased (TrxDbFileBasedEnv "dir") $ do
                writeAllTrx ts
                ts' <- readAllTrx
                pure (ts == ts')
