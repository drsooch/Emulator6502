-- | Logical Operations Testing

module Test.LogicalTest
    ( logical
    ) where

import           Data.Functor                   ( (<&>) )
import           Execution
import           Flags
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , assertBool
                                                , testCase
                                                )
import           TestUtils
import           Types

logical :: TestTree
logical = testGroup "Logical Operations"
                    [logicalAnd, logicalXor, logicalOr, logicalBit]

{--------------------------------------  AND Tests --------------------------------------}

logicalAnd :: TestTree
logicalAnd = testGroup
    "Logical And Operations"
    [ logicalAndImmediate
    , logicalAndZeroPage
    , logicalAndZeroPageX
    , logicalAndAbsolute
    , logicalAndAbsoluteX
    , logicalAndAbsoluteY
    , logicalAndIndirectX
    , logicalAndIndirectY
    ]

logicalAndImmediate :: TestTree
logicalAndImmediate = testCase "Logical And - Immediate" $ do
    cpu <-
        mkTestCPU "Logical And - Immediate"
        <&> setProgramMemory [0x29, 0x57]
        .   setARegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x17 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndZeroPage :: TestTree
logicalAndZeroPage = testCase "Logical And - ZeroPage" $ do
    cpu <-
        mkTestCPU "Logical And - ZeroPage"
        <&> setProgramMemory [0x25, 0x57]
        .   setARegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x8 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndZeroPageX :: TestTree
logicalAndZeroPageX = testCase "Logical And - ZeroPage - X" $ do
    cpu <-
        mkTestCPU "Logical And - ZeroPage - X"
        <&> setProgramMemory [0x35, 0x1A]
        .   setARegisterTest 0x1F
        .   setXRegisterTest 0xD5
    (_, cpu') <- runEmulatorTest execute cpu
    0x10 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndAbsolute :: TestTree
logicalAndAbsolute = testCase "Logical And - Absolute" $ do
    cpu <-
        mkTestCPU "Logical And - Absolute"
        <&> setTestMemory 0x4000 [0x4A]
        .   setProgramMemory [0x2D, 0x0, 0x40]
        .   setARegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0xA @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndAbsoluteX :: TestTree
logicalAndAbsoluteX = testCase "Logical And - Absolute - X" $ do
    cpu <-
        mkTestCPU "Logical And - Absolute - X"
        <&> setTestMemory 0x40AA [0x4A]
        .   setProgramMemory [0x3D, 0x0, 0x40]
        .   setARegisterTest 0x1F
        .   setXRegisterTest 0xAA
    (_, cpu') <- runEmulatorTest execute cpu
    0xA @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndAbsoluteY :: TestTree
logicalAndAbsoluteY = testCase "Logical And - Absolute - Y" $ do
    cpu <-
        mkTestCPU "Logical And - Absolute - Y"
        <&> setTestMemory 0x40C1 [0x5F]
        .   setProgramMemory [0x39, 0x0, 0x40]
        .   setARegisterTest 0xB2
        .   setYRegisterTest 0xC1
    (_, cpu') <- runEmulatorTest execute cpu
    0x12 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndIndirectX :: TestTree
logicalAndIndirectX = testCase "Logical And - Indirect - X" $ do
    cpu <-
        mkTestCPU "Logical And - Indirect - X"
        <&> setTestMemory 0x4344 [0xC]
        .   setProgramMemory [0x21, 0x0]
        .   setARegisterTest 0xC4
        .   setXRegisterTest 0xBB

    (_, cpu') <- runEmulatorTest execute cpu
    0x04 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndIndirectY :: TestTree
logicalAndIndirectY = testCase "Logical And - Indirect - Y" $ do
    cpu <-
        mkTestCPU "Logical And - Indirect - Y"
        <&> setTestMemory 0xA863 [0xFF]
        .   setProgramMemory [0x31, 0x57]
        .   setARegisterTest 0xD1
        .   setYRegisterTest 0xBB
    (_, cpu') <- runEmulatorTest execute cpu
    0xD1 @=? aReg cpu'
    negFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}

logicalXor :: TestTree
logicalXor = testGroup
    "Logical XOR Operations"
    [ logicalXorImmediate
    , logicalXorImmediateZeroFlag
    , logicalXorZeroPage
    , logicalXorZeroPageX
    , logicalXorAbsolute
    , logicalXorAbsoluteX
    , logicalXorAbsoluteY
    , logicalXorIndirectX
    , logicalXorIndirectY
    ]

logicalXorImmediate :: TestTree
logicalXorImmediate = testCase "Logical XOR - Immediate" $ do
    cpu <-
        mkTestCPU "Logical XOR - Immediate"
        <&> setProgramMemory [0x49, 0xB7]
        .   setARegisterTest 0xBC
    (_, cpu') <- runEmulatorTest execute cpu
    0xB @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalXorImmediateZeroFlag :: TestTree
logicalXorImmediateZeroFlag =
    testCase "Logical XOR - Immediate - Zero Flag" $ do
        cpu <-
            mkTestCPU "Logical XOR - Immediate - ZeroFlag"
            <&> setProgramMemory [0x49, 0x00]
            .   setARegisterTest 0x00
        (_, cpu') <- runEmulatorTest execute cpu
        0x0 @=? aReg cpu'
        zeroFlag @=? fReg cpu'

logicalXorZeroPage :: TestTree
logicalXorZeroPage = testCase "Logical XOR - ZeroPage" $ do
    cpu <-
        mkTestCPU "Logical XOR - ZeroPage"
        <&> setProgramMemory [0x45, 0x57]
        .   setARegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0xB7 @=? aReg cpu'
    negFlag @=? fReg cpu'

logicalXorZeroPageX :: TestTree
logicalXorZeroPageX = testCase "Logical XOR - ZeroPage X" $ do
    cpu <-
        mkTestCPU "Logical XOR - ZeroPage X"
        <&> setProgramMemory [0x55, 0x57]
        .   setARegisterTest 0xCC
        .   setXRegisterTest 0x4A
    (_, cpu') <- runEmulatorTest execute cpu
    0x92 @=? aReg cpu'
    negFlag @=? fReg cpu'

logicalXorAbsolute :: TestTree
logicalXorAbsolute = testCase "Logical XOR - Absolute" $ do
    cpu <-
        mkTestCPU "Logical XOR - Absolute"
        <&> setTestMemory 0x5FEE [0xEA]
        .   setProgramMemory [0x4D, 0xEE, 0x5F]
        .   setARegisterTest 0x05
    (_, cpu') <- runEmulatorTest execute cpu
    0xEF @=? aReg cpu'
    negFlag @=? fReg cpu'

logicalXorAbsoluteX :: TestTree
logicalXorAbsoluteX = testCase "Logical XOR - Absolute X" $ do
    cpu <-
        mkTestCPU "Logical XOR - Absolute X"
        <&> setTestMemory 0x3092 [0x7B]
        .   setProgramMemory [0x5D, 0x00, 0x30]
        .   setARegisterTest 0x2A
        .   setXRegisterTest 0x92
    (_, cpu') <- runEmulatorTest execute cpu
    0x51 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalXorAbsoluteY :: TestTree
logicalXorAbsoluteY = testCase "Logical XOR - Absolute Y" $ do
    cpu <-
        mkTestCPU "Logical XOR - Absolute Y"
        <&> setTestMemory 0x40F2 [0x7B]
        .   setProgramMemory [0x59, 0x00, 0x40]
        .   setARegisterTest 0x2A
        .   setYRegisterTest 0xF2
    (_, cpu') <- runEmulatorTest execute cpu
    0x51 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalXorIndirectX :: TestTree
logicalXorIndirectX = testCase "Logical XOR - Indirect X" $ do
    cpu <-
        mkTestCPU "Logical XOR - Indirect X"
        <&> setTestMemory 0xDADB [0xDB]
        .   setProgramMemory [0x41, 0x32]
        .   setARegisterTest 0xCC
        .   setXRegisterTest 0xF2
    (_, cpu') <- runEmulatorTest execute cpu
    0x17 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalXorIndirectY :: TestTree
logicalXorIndirectY = testCase "Logical XOR - Indirect Y" $ do
    cpu <-
        mkTestCPU "Logical XOR - Indirect Y"
        <&> setTestMemory 0xCD0D [0xB1]
        .   setProgramMemory [0x51, 0x32]
        .   setARegisterTest 0x11
        .   setYRegisterTest 0x40
    (_, cpu') <- runEmulatorTest execute cpu
    0xA0 @=? aReg cpu'
    negFlag @=? fReg cpu'

{--------------------------------------  ORA Tests --------------------------------------}
logicalOr :: TestTree
logicalOr = testGroup
    "Logical Or Operations"
    [ logicalOrImmediate
    , logicalOrZeroPage
    , logicalOrZeroPageX
    , logicalOrAbsolute
    , logicalOrAbsoluteX
    , logicalOrAbsoluteY
    , logicalOrIndirectX
    , logicalOrIndirectY
    ]

logicalOrImmediate :: TestTree
logicalOrImmediate = testCase "Logical Or - Immediate" $ do
    cpu <-
        mkTestCPU "Logical Or - Immediate"
        <&> setProgramMemory [0x09, 0x57]
        .   setARegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x5F @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalOrZeroPage :: TestTree
logicalOrZeroPage = testCase "Logical Or - ZeroPage" $ do
    cpu <-
        mkTestCPU "Logical Or - ZeroPage"
        <&> setProgramMemory [0x05, 0x57]
        .   setARegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0xBF @=? aReg cpu'
    negFlag @=? fReg cpu'

logicalOrZeroPageX :: TestTree
logicalOrZeroPageX = testCase "Logical Or - ZeroPage - X" $ do
    cpu <-
        mkTestCPU "Logical Or - ZeroPage - X"
        <&> setProgramMemory [0x15, 0x1A]
        .   setARegisterTest 0x1F
        .   setXRegisterTest 0xD5
    (_, cpu') <- runEmulatorTest execute cpu
    0x1F @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalOrAbsolute :: TestTree
logicalOrAbsolute = testCase "Logical Or - Absolute" $ do
    cpu <-
        mkTestCPU "Logical Or - Absolute"
        <&> setTestMemory 0x4000 [0x4A]
        .   setProgramMemory [0xD, 0x0, 0x40]
        .   setARegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x5F @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalOrAbsoluteX :: TestTree
logicalOrAbsoluteX = testCase "Logical Or - Absolute - X" $ do
    cpu <-
        mkTestCPU "Logical Or - Absolute - X"
        <&> setTestMemory 0x40AA [0x4A]
        .   setProgramMemory [0x1D, 0x0, 0x40]
        .   setARegisterTest 0x1F
        .   setXRegisterTest 0xAA
    (_, cpu') <- runEmulatorTest execute cpu
    0x5F @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalOrAbsoluteY :: TestTree
logicalOrAbsoluteY = testCase "Logical Or - Absolute - Y" $ do
    cpu <-
        mkTestCPU "Logical Or - Absolute - Y"
        <&> setTestMemory 0x40C1 [0x5F]
        .   setProgramMemory [0x19, 0x0, 0x40]
        .   setARegisterTest 0xB2
        .   setYRegisterTest 0xC1
    (_, cpu') <- runEmulatorTest execute cpu
    0xFF @=? aReg cpu'
    negFlag @=? fReg cpu'

logicalOrIndirectX :: TestTree
logicalOrIndirectX = testCase "Logical Or - Indirect - X" $ do
    cpu <-
        mkTestCPU "Logical Or - Indirect - X"
        <&> setTestMemory 0x4344 [0xC]
        .   setProgramMemory [0x1, 0x0]
        .   setARegisterTest 0xC4
        .   setXRegisterTest 0xBB
    (_, cpu') <- runEmulatorTest execute cpu
    0xCC @=? aReg cpu'
    negFlag @=? fReg cpu'

logicalOrIndirectY :: TestTree
logicalOrIndirectY = testCase "Logical Or - Indirect - Y" $ do
    cpu <-
        mkTestCPU "Logical Or - Indirect - Y"
        <&> setTestMemory 0xA863 [0xE1]
        .   setProgramMemory [0x11, 0x57]
        .   setARegisterTest 0xD1
        .   setYRegisterTest 0xBB
    (_, cpu') <- runEmulatorTest execute cpu
    0xF1 @=? aReg cpu'
    negFlag @=? fReg cpu'

{--------------------------------------  BIT Tests --------------------------------------}

logicalBit :: TestTree
logicalBit =
    testGroup "Logical Bit Operations" [logicalBitZeroPage, logicalBitAbsolute]

logicalBitZeroPage :: TestTree
logicalBitZeroPage = testCase "Logical Bit - ZeroPage" $ do
    cpu <-
        mkTestCPU "LogicalBitZeroPage"
        <&> setProgramMemory [0x24, 0xF2]
        .   setARegisterTest 0xF2
    (_, cpu') <- runEmulatorTest execute cpu
    assertBool "Checking Zero Bit Set"       (isFlagSet' (fReg cpu') ZF)
    assertBool "Checking Overflow Bit Clear" (not $ isFlagSet' (fReg cpu') OF)
    assertBool "Checking Negative Bit Clear" (not $ isFlagSet' (fReg cpu') NF)


logicalBitAbsolute :: TestTree
logicalBitAbsolute = testCase "Logical Bit - Absolute" $ do
    cpu <-
        mkTestCPU "LogicalBitAbsolute"
        <&> setProgramMemory [0x2C, 0x19]
        .   setARegisterTest 0xE6
    (_, cpu') <- runEmulatorTest execute cpu
    assertBool "Checking Zero Bit Clear"   (not $ isFlagSet' (fReg cpu') ZF)
    assertBool "Checking Overflow Bit Set" (isFlagSet' (fReg cpu') OF)
    assertBool "Checking Negative Bit Set" (isFlagSet' (fReg cpu') NF)
