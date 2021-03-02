import           LoadStoreTest
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Full" [loadStore]
