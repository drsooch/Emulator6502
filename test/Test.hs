import           ADCTest
import           AnalyzeTest
import           BranchTest
import           CompareTest
import           DecrementTest
import           IncrementTest
import           LoadStoreTest
import           LogicalTest
import           ParserTest
import           ShiftTest
import           StackTest
import           StatusFlagTest
import           System.Directory               ( getCurrentDirectory
                                                , listDirectory
                                                , removeFile
                                                )
import           System.Environment             ( getArgs
                                                , setEnv
                                                )
import           Test.Tasty
import           TransferTest

main :: IO ()
main = do
    setEnv "TASTY_QUICKCHECK_VERBOSE" "True"
    purgeTempLogs
    getArgs >>= \case
        ["full"] -> defaultMain fullSuite
        _        -> defaultMain analysis

fullSuite :: TestTree
fullSuite = testGroup
    "Full Test Suite"
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
    , parsing
    , analysis
    ]

purgeTempLogs :: IO ()
purgeTempLogs = getCurrentDirectory >>= \cwd ->
    listDirectory (cwd <> "/log") >>= \fs -> mapM_ (removeFile . (<>) (cwd <> "/log/")) fs
