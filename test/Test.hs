import           ADCTest
import           BranchTest
import           CompareTest
import           DecrementTest
import           IncrementTest
import           LoadStoreTest
import           LogicalTest
import           ShiftTest
import           StackTest
import           StatusFlagTest
import           System.Directory               ( getCurrentDirectory
                                                , listDirectory
                                                , removeFile
                                                )
--import           System.Environment             ( setEnv )
import           Test.Tasty
import           TransferTest

main :: IO ()
main = do
    purgeTempLogs
    --setEnv "TASTY_QUICKCHECK_VERBOSE" "True"
    defaultMain tests

tests :: TestTree
tests = testGroup
    "Full"
    [ loadStore
    , transfer
    , stackOps
    , logical
    , adcCarry
    , compareOps
    , increment
    , decrement
    , shifts
    , branches
    , statusFlags
    ]

purgeTempLogs :: IO ()
purgeTempLogs = getCurrentDirectory >>= \cwd ->
    listDirectory (cwd <> "/log") >>= \fs -> mapM_ (removeFile . (<>) (cwd <> "/log/")) fs
