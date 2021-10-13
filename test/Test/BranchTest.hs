-- | Branch Operation Testing
module Test.BranchTest
    ( branches
    ) where

import           Data.Functor                   ( (<&>) )
import           Execution
import           Flags
import           ProgramCounter
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , testCase
                                                )
import           Test.TestUtils
import           Types

branches :: TestTree
branches = testGroup
    "Branch Operations"
    [ bccTestClear
    , bccTestSet
    , bcsTestClear
    , bcsTestSet
    , beqTestClear
    , beqTestSet
    , bneTestClear
    , bneTestSet
    , bmiTestClear
    , bmiTestSet
    , bplTestClear
    , bplTestSet
    , bvcTestClear
    , bvcTestSet
    , bvsTestClear
    , bvsTestSet
    ]

bccTestClear :: TestTree
bccTestClear = testCase "Branch Carry Clear - Clear" $ do
    cpu      <- mkTestCPU "BranchCarryClearClear" <&> setProgramMemory [0x90, 0x15]
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x17 @=? pc'

bccTestSet :: TestTree
bccTestSet = testCase "Branch Carry Clear - Set" $ do
    cpu <-
        mkTestCPU "BranchCarryClearSet"
        <&> setProgramMemory [0x90, 0x15]
        .   setFRegisterTest carryFlag
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x2 @=? pc'

bcsTestClear :: TestTree
bcsTestClear = testCase "Branch Carry Set - Clear" $ do
    cpu      <- mkTestCPU "BranchCarrySetClear" <&> setProgramMemory [0xB0, 0x15]
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x2 @=? pc'

bcsTestSet :: TestTree
bcsTestSet = testCase "Branch Carry Set - Set" $ do
    cpu <-
        mkTestCPU "BranchCarrySetSet" <&> setProgramMemory [0xB0, 0x15] . setFRegisterTest carryFlag
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x17 @=? pc'

beqTestClear :: TestTree
beqTestClear = testCase "Branch Zero Set - Clear" $ do
    cpu      <- mkTestCPU "BranchZeroSetClear" <&> setProgramMemory [0xF0, 0x15]
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x2 @=? pc'

beqTestSet :: TestTree
beqTestSet = testCase "Branch Zero Set - Set" $ do
    cpu <-
        mkTestCPU "BranchZeroSetSet" <&> setProgramMemory [0xF0, 0x15] . setFRegisterTest zeroFlag
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x17 @=? pc'

bneTestClear :: TestTree
bneTestClear = testCase "Branch Zero Clear - Clear" $ do
    cpu      <- mkTestCPU "BranchZeroClearClear" <&> setProgramMemory [0xD0, 0x15]
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x17 @=? pc'

bneTestSet :: TestTree
bneTestSet = testCase "Branch Zero Clear - Set" $ do
    cpu <-
        mkTestCPU "BranchZeroClearSet" <&> setProgramMemory [0xD0, 0x15] . setFRegisterTest zeroFlag
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x2 @=? pc'

bmiTestClear :: TestTree
bmiTestClear = testCase "Branch Negative Set - Clear" $ do
    cpu      <- mkTestCPU "BranchNegativeSetClear" <&> setProgramMemory [0x30, 0x15]
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x2 @=? pc'

bmiTestSet :: TestTree
bmiTestSet = testCase "Branch Negative Set - Set" $ do
    cpu <-
        mkTestCPU "BranchNegativeSetSet"
        <&> setProgramMemory [0x30, 0x15]
        .   setFRegisterTest negFlag
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x17 @=? pc'

bplTestClear :: TestTree
bplTestClear = testCase "Branch Negative Clear - Clear" $ do
    cpu      <- mkTestCPU "BranchNegativeClearClear" <&> setProgramMemory [0x10, 0x15]
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x17 @=? pc'

bplTestSet :: TestTree
bplTestSet = testCase "Branch Negative Clear - Set" $ do
    cpu <-
        mkTestCPU "BranchNegativeClearSet"
        <&> setProgramMemory [0x10, 0x15]
        .   setFRegisterTest negFlag
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x2 @=? pc'

bvcTestClear :: TestTree
bvcTestClear = testCase "Branch Overflow Clear - Clear" $ do
    cpu      <- mkTestCPU "BranchOverflowClearClear" <&> setProgramMemory [0x50, 0x15]
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x17 @=? pc'

bvcTestSet :: TestTree
bvcTestSet = testCase "Branch Overflow Clear - Set" $ do
    cpu <-
        mkTestCPU "BranchOverflowClearSet"
        <&> setProgramMemory [0x50, 0x15]
        .   setFRegisterTest ovFlag
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x2 @=? pc'

bvsTestClear :: TestTree
bvsTestClear = testCase "Branch Overflow Set - Clear" $ do
    cpu      <- mkTestCPU "BranchOverflowSetClear" <&> setProgramMemory [0x70, 0x15]
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x2 @=? pc'

bvsTestSet :: TestTree
bvsTestSet = testCase "Branch Overflow Set - Set" $ do
    cpu <-
        mkTestCPU "BranchOverflowSetSet" <&> setProgramMemory [0x70, 0x15] . setFRegisterTest ovFlag
    (pc', _) <- runEmulatorTest (execute >> getProgramCounter) cpu
    getPC (pc cpu) + 0x17 @=? pc'
