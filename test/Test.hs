import           ADCTest
import           LoadStoreTest
import           LogicalTest
import           StackTest
import           System.Directory
import           Test.Tasty
import           TransferTest

main :: IO ()
main = do
    purgeTempLogs
    defaultMain tests

tests :: TestTree
tests = testGroup "Full" [loadStore, transfer, stackOps, logical, adcCarry]

purgeTempLogs :: IO ()
purgeTempLogs = getCurrentDirectory >>= \cwd -> listDirectory (cwd <> "/log")
    >>= \fs -> mapM_ (removeFile . (<>) (cwd <> "/log/")) fs
