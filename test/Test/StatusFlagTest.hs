-- | Status Flag Operations

module Test.StatusFlagTest
    ( statusFlags
    ) where

import           Data.Functor                   ( (<&>) )
import           Execution
import           Flags
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , testCase
                                                )
import           TestUtils

statusFlags :: TestTree
statusFlags = testGroup
    "Status Flag Operations"
    [ clcSet
    , clcClear
    , cldSet
    , cldClear
    , cliSet
    , cliClear
    , clvSet
    , clvClear
    , secSet
    , secClear
    , sedSet
    , sedClear
    , seiSet
    , seiClear
    ]

clcSet :: TestTree
clcSet = testCase "Clear Carry Flag - Set" $ do
    cpu <-
        mkTestCPU "ClearCarryFlagSet"
        <&> setProgramMemory [0x18]
        .   setFRegisterTest carryFlag
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    0x0 @=? fReg'

clcClear :: TestTree
clcClear = testCase "Clear Carry Flag - Clear" $ do
    cpu        <- mkTestCPU "ClearCarryFlagClear" <&> setProgramMemory [0x18]
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    0x0 @=? fReg'

cldSet :: TestTree
cldSet = testCase "Clear Decimal Flag - Set" $ do
    cpu <-
        mkTestCPU "ClearDecimalFlagSet"
        <&> setProgramMemory [0xD8]
        .   setFRegisterTest decFlag
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    0x0 @=? fReg'

cldClear :: TestTree
cldClear = testCase "Clear Decimal Flag - Clear" $ do
    cpu        <- mkTestCPU "ClearDecimalFlagClear" <&> setProgramMemory [0xD8]
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    0x0 @=? fReg'

cliSet :: TestTree
cliSet = testCase "Clear Interrupt Flag - Set" $ do
    cpu <-
        mkTestCPU "ClearInterruptFlagSet"
        <&> setProgramMemory [0x58]
        .   setFRegisterTest intFlag
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    0x0 @=? fReg'

cliClear :: TestTree
cliClear = testCase "Clear Interrupt Flag - Clear" $ do
    cpu <- mkTestCPU "ClearInterruptFlagClear" <&> setProgramMemory [0x58]
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    0x0 @=? fReg'

clvSet :: TestTree
clvSet = testCase "Clear Overflow Flag - Set" $ do
    cpu <-
        mkTestCPU "ClearOverflowFlagSet"
        <&> setProgramMemory [0xB8]
        .   setFRegisterTest ovFlag
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    0x0 @=? fReg'

clvClear :: TestTree
clvClear = testCase "Clear Overflow Flag - Clear" $ do
    cpu        <- mkTestCPU "ClearOverflowFlagClear" <&> setProgramMemory [0xB8]
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    0x0 @=? fReg'

secSet :: TestTree
secSet = testCase "Set Carry Flag - Set" $ do
    cpu <-
        mkTestCPU "SetCarryFlagSet"
        <&> setProgramMemory [0x38]
        .   setFRegisterTest carryFlag
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

secClear :: TestTree
secClear = testCase "Set Carry Flag - Clear" $ do
    cpu        <- mkTestCPU "SetCarryFlagClear" <&> setProgramMemory [0x38]
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

sedSet :: TestTree
sedSet = testCase "Set Decimal Flag - Set" $ do
    cpu <-
        mkTestCPU "SetDecimalFlagSet"
        <&> setProgramMemory [0xF8]
        .   setFRegisterTest decFlag
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    decFlag @=? fReg'

sedClear :: TestTree
sedClear = testCase "Set Decimal Flag - Clear" $ do
    cpu        <- mkTestCPU "SetDecimalFlagClear" <&> setProgramMemory [0xF8]
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    decFlag @=? fReg'

seiSet :: TestTree
seiSet = testCase "Set Interrupt Flag - Set" $ do
    cpu <-
        mkTestCPU "SetInterruptFlagSet"
        <&> setProgramMemory [0x78]
        .   setFRegisterTest intFlag
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    intFlag @=? fReg'

seiClear :: TestTree
seiClear = testCase "Set Interrupt Flag - Clear" $ do
    cpu        <- mkTestCPU "SetInterruptFlagClear" <&> setProgramMemory [0x78]
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    intFlag @=? fReg'
