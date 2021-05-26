import           ADCTest
import           LoadStoreTest
import           LogicalTest
import           StackTest
import           Test.Tasty
import           TransferTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Full" [loadStore, transfer, stackOps, logical, adcCarry]
