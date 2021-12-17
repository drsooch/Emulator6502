import          Test.ADCTest
import          Test.Assembler.AnalyzeTest
import          Test.BranchTest
import          Test.CompareTest
import          Test.DecrementTest
import          Test.IncrementTest
import          Test.LoadStoreTest
import          Test.LogicalTest
import          Test.ParserTest
import          Test.ShiftTest
import          Test.StackTest
import          Test.StatusFlagTest
import           System.Directory               ( getCurrentDirectory
                                                , listDirectory
                                                , removeFile
                                                )
import           System.Environment             ( getArgs
                                                , setEnv
                                                )
import           Test.Tasty
import           Test.TransferTest

main :: IO ()
main = do
    setEnv "TASTY_QUICKCHECK_VERBOSE" "False"
    purgeTempLogs
    getArgs >>= \case
        ["full"] -> defaultMain fullSuite
        _        -> defaultMain fullSuite

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
