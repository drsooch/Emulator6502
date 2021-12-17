-- | Comparison Operations

module Test.CompareTest
    ( compareOps
    ) where

import           Data.Bits                      ( (.|.) )
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

compareOps :: TestTree
compareOps = testGroup "Comparison Operations" [cmpOps, cpxOps, cpyOps]

{--------------------------------------  CMP Tests --------------------------------------}
cmpOps :: TestTree
cmpOps = testGroup
    "CMP Operations"
    [ cmpLT
    , cmpEQ
    , cmpGT
    , cmpGE
    , cmpZeroPage
    , cmpZeroPageX
    , cmpAbsolute
    , cmpAbsoluteX
    , cmpAbsoluteY
    , cmpIndirectX
    , cmpIndirectY
    ]

cmpIndirectY :: TestTree
cmpIndirectY = testCase "CMP - Indirect - Y" $ do
    cpu <-
        mkTestCPU "CMPIndirectY"
        <&> setTestMemory 0xEEFF [0x77]
        .   setProgramMemory [0xD1, 0x10]
        .   setARegisterTest 0x44
        .   setYRegisterTest 0x10
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    negFlag @=? fReg'

cmpIndirectX :: TestTree
cmpIndirectX = testCase "CMP - Indirect - X" $ do
    cpu <-
        mkTestCPU "CMPIndirectX"
        <&> setTestMemory 0x4344 [0xC]
        .   setProgramMemory [0xC1, 0x0]
        .   setARegisterTest 0xC4
        .   setXRegisterTest 0xBB
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    (carryFlag .|. negFlag) @=? fReg'


cmpAbsoluteX :: TestTree
cmpAbsoluteX = testCase "CMP - Absolute - X" $ do
    cpu <-
        mkTestCPU "CMPAbsoluteX"
        <&> setTestMemory 0x43FF [0xC]
        .   setProgramMemory [0xDD, 0x44, 0x43]
        .   setARegisterTest 0x4
        .   setXRegisterTest 0xBB
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    negFlag @=? fReg'

cmpAbsoluteY :: TestTree
cmpAbsoluteY = testCase "CMP - Absolute - Y" $ do
    cpu <-
        mkTestCPU "CMPAbsoluteY"
        <&> setTestMemory 0x43FF [0xC]
        .   setProgramMemory [0xD9, 0x44, 0x43]
        .   setARegisterTest 0xF
        .   setYRegisterTest 0xBB
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cmpAbsolute :: TestTree
cmpAbsolute = testCase "CMP - Absolute" $ do
    cpu <-
        mkTestCPU "CMPAbsolute"
        <&> setTestMemory 0x43FF [0xC]
        .   setProgramMemory [0xCD, 0xFF, 0x43]
        .   setARegisterTest 0xF
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cmpZeroPageX :: TestTree
cmpZeroPageX = testCase "CMP - ZeroPage - X" $ do
    cpu <-
        mkTestCPU "CMPZeroPageX"
        <&> setProgramMemory [0xD5, 0xC1]
        .   setARegisterTest 0x57
        .   setXRegisterTest 0x55
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    0x0 @=? fReg'

cmpZeroPage :: TestTree
cmpZeroPage = testCase "CMP - ZeroPage" $ do
    cpu <-
        mkTestCPU "CMPZeroPage"
        <&> setProgramMemory [0xC5, 0xC1]
        .   setARegisterTest 0x57
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cmpGE :: TestTree
cmpGE = testCase "CMP Greater Equal" $ do
    cpu <-
        mkTestCPU "CMPGreaterEqual"
        <&> setProgramMemory [0xC9, 0x44]
        .   setARegisterTest 0x44
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    (carryFlag .|. zeroFlag) @=? fReg'

cmpGT :: TestTree
cmpGT = testCase "CMP Greater Than" $ do
    cpu <-
        mkTestCPU "CMPGreaterThan"
        <&> setProgramMemory [0xC9, 0x44]
        .   setARegisterTest 0x45
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cmpEQ :: TestTree
cmpEQ = testCase "CMP Equal" $ do
    cpu <-
        mkTestCPU "CMPEqual"
        <&> setProgramMemory [0xC9, 0xFF]
        .   setARegisterTest 0xFF
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    (zeroFlag .|. carryFlag) @=? fReg'

cmpLT :: TestTree
cmpLT = testCase "CMP Less Than" $ do
    cpu <-
        mkTestCPU "CMPLessThan"
        <&> setProgramMemory [0xC9, 0x44]
        .   setARegisterTest 0x10
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    negFlag @=? fReg'

{--------------------------------------  CPX Tests --------------------------------------}
cpxOps :: TestTree
cpxOps = testGroup "CPX Operations"
                   [cpxLT, cpxEQ, cpxGT, cpxGE, cpxZeroPage, cpxAbsolute]


cpxAbsolute :: TestTree
cpxAbsolute = testCase "CPX - Absolute" $ do
    cpu <-
        mkTestCPU "CPXAbsolute"
        <&> setTestMemory 0x43FF [0xC]
        .   setProgramMemory [0xEC, 0xFF, 0x43]
        .   setXRegisterTest 0xF
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cpxZeroPage :: TestTree
cpxZeroPage = testCase "CPX - ZeroPage" $ do
    cpu <-
        mkTestCPU "CPXZeroPage"
        <&> setProgramMemory [0xE4, 0xC1]
        .   setXRegisterTest 0x57
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cpxGE :: TestTree
cpxGE = testCase "CPX Greater Equal" $ do
    cpu <-
        mkTestCPU "CPXGreaterEqual"
        <&> setProgramMemory [0xE0, 0x44]
        .   setXRegisterTest 0x44
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    (carryFlag .|. zeroFlag) @=? fReg'

cpxGT :: TestTree
cpxGT = testCase "CPX Greater Than" $ do
    cpu <-
        mkTestCPU "CPXGreaterThan"
        <&> setProgramMemory [0xE0, 0x44]
        .   setXRegisterTest 0x45
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cpxEQ :: TestTree
cpxEQ = testCase "CPX Equal" $ do
    cpu <-
        mkTestCPU "CPXEqual"
        <&> setProgramMemory [0xE0, 0xFF]
        .   setXRegisterTest 0xFF
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    (zeroFlag .|. carryFlag) @=? fReg'

cpxLT :: TestTree
cpxLT = testCase "CPX Less Than" $ do
    cpu <-
        mkTestCPU "CPXLessThan"
        <&> setProgramMemory [0xE0, 0x44]
        .   setXRegisterTest 0x10
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    negFlag @=? fReg'

{--------------------------------------  CPY Tests --------------------------------------}
cpyOps :: TestTree
cpyOps = testGroup "CPY Operations"
                   [cpyLT, cpyEQ, cpyGT, cpyGE, cpyZeroPage, cpyAbsolute]

cpyAbsolute :: TestTree
cpyAbsolute = testCase "CPY - Absolute" $ do
    cpu <-
        mkTestCPU "CPYAbsolute"
        <&> setTestMemory 0x43FF [0xC]
        .   setProgramMemory [0xCC, 0xFF, 0x43]
        .   setYRegisterTest 0xF
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cpyZeroPage :: TestTree
cpyZeroPage = testCase "CPY - ZeroPage" $ do
    cpu <-
        mkTestCPU "CPYZeroPage"
        <&> setProgramMemory [0xC4, 0xC1]
        .   setYRegisterTest 0x57
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cpyGE :: TestTree
cpyGE = testCase "CPY Greater Equal" $ do
    cpu <-
        mkTestCPU "CPYGreaterEqual"
        <&> setProgramMemory [0xC0, 0x44]
        .   setYRegisterTest 0x44
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    (carryFlag .|. zeroFlag) @=? fReg'

cpyGT :: TestTree
cpyGT = testCase "CPY Greater Than" $ do
    cpu <-
        mkTestCPU "CPYGreaterThan"
        <&> setProgramMemory [0xC0, 0x44]
        .   setYRegisterTest 0x45
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    carryFlag @=? fReg'

cpyEQ :: TestTree
cpyEQ = testCase "CPY Equal" $ do
    cpu <-
        mkTestCPU "CPYEqual"
        <&> setProgramMemory [0xC0, 0xFF]
        .   setYRegisterTest 0xFF
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    (zeroFlag .|. carryFlag) @=? fReg'

cpyLT :: TestTree
cpyLT = testCase "CPY Less Than" $ do
    cpu <-
        mkTestCPU "CPYLessThan"
        <&> setProgramMemory [0xC0, 0x44]
        .   setYRegisterTest 0x10
    (fReg', _) <- runEmulatorTest (execute >> getFRegister) cpu
    negFlag @=? fReg'
