-- | TAX, TAY, TXA, TYA

module Test.TransferTest
    ( transfer
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
import           Types

transfer :: TestTree
transfer = testGroup "Transfer Operations"
                     [transferTAX, transferTAY, transferTXA, transferTYA]

{--------------------------------------  TAX Transfer Tests --------------------------------------}
transferTAX :: TestTree
transferTAX = testGroup
    "Transfer Operations: A to X"
    [transferTAXNegative, transferTAXPositive, transferTAXZero]

transferTAXNegative :: TestTree
transferTAXNegative = testCase "Transfer TAX - Negative" $ do
    cpu <-
        mkTestCPU "Transfer TAX - Negative"
        <&> setProgramMemory [0xAA]
        .   setARegisterTest 0xCA
        .   setXRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0xCA @=? xReg cpu'
    negFlag @=? fReg cpu'

transferTAXPositive :: TestTree
transferTAXPositive = testCase "Transfer TAX - Positive" $ do
    cpu <-
        mkTestCPU "Transfer TAX - Positive"
        <&> setProgramMemory [0xAA]
        .   setARegisterTest 0x64
        .   setXRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x64 @=? xReg cpu'
    0x0 @=? fReg cpu'

transferTAXZero :: TestTree
transferTAXZero = testCase "Transfer TAX - Zero" $ do
    cpu <-
        mkTestCPU "Transfer TAX - Zero"
        <&> setProgramMemory [0xAA]
        .   setARegisterTest 0x0
        .   setXRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x0 @=? xReg cpu'
    zeroFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}

{--------------------------------------  TAY Transfer Tests --------------------------------------}
transferTAY :: TestTree
transferTAY = testGroup
    "Transfer Operations: A to Y"
    [transferTAYNegative, transferTAYPositive, transferTAYZero]

transferTAYNegative :: TestTree
transferTAYNegative = testCase "Transfer TAY - Negative" $ do
    cpu <-
        mkTestCPU "Transfer TAY - Negative"
        <&> setProgramMemory [0xA8]
        .   setARegisterTest 0xCA
        .   setYRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0xCA @=? yReg cpu'
    negFlag @=? fReg cpu'

transferTAYPositive :: TestTree
transferTAYPositive = testCase "Transfer TAY - Positive" $ do
    cpu <-
        mkTestCPU "Transfer TAY - Positive"
        <&> setProgramMemory [0xA8]
        .   setARegisterTest 0x64
        .   setYRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x64 @=? yReg cpu'
    0x0 @=? fReg cpu'

transferTAYZero :: TestTree
transferTAYZero = testCase "Transfer TAY - Zero" $ do
    cpu <-
        mkTestCPU "Transfer TAY - Zero"
        <&> setProgramMemory [0xA8]
        .   setARegisterTest 0x0
        .   setYRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x0 @=? yReg cpu'
    zeroFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}

{--------------------------------------  TXA Transfer Tests --------------------------------------}
transferTXA :: TestTree
transferTXA = testGroup
    "Transfer Operations: X to A"
    [transferTXANegative, transferTXAPositive, transferTXAZero]

transferTXANegative :: TestTree
transferTXANegative = testCase "Transfer TXA - Negative" $ do
    cpu <-
        mkTestCPU "Transfer TXA - Negative"
        <&> setProgramMemory [0x8A]
        .   setARegisterTest 0x1F
        .   setXRegisterTest 0xCA
    (_, cpu') <- runEmulatorTest execute cpu
    0xCA @=? aReg cpu'
    negFlag @=? fReg cpu'

transferTXAPositive :: TestTree
transferTXAPositive = testCase "Transfer TXA - Positive" $ do
    cpu <-
        mkTestCPU "Transfer TXA - Positive"
        <&> setProgramMemory [0x8A]
        .   setARegisterTest 0x1F
        .   setXRegisterTest 0x64
    (_, cpu') <- runEmulatorTest execute cpu
    0x64 @=? aReg cpu'
    0x0 @=? fReg cpu'

transferTXAZero :: TestTree
transferTXAZero = testCase "Transfer TXA - Zero" $ do
    cpu <-
        mkTestCPU "Transfer TXA - Zero"
        <&> setProgramMemory [0x8A]
        .   setARegisterTest 0x1F
        .   setXRegisterTest 0x0
    (_, cpu') <- runEmulatorTest execute cpu
    0x0 @=? aReg cpu'
    zeroFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}

{--------------------------------------  TYA Transfer Tests --------------------------------------}
transferTYA :: TestTree
transferTYA = testGroup
    "Transfer Operations: Y to A"
    [transferTYANegative, transferTYAPositive, transferTYAZero]

transferTYANegative :: TestTree
transferTYANegative = testCase "Transfer TYA - Negative" $ do
    cpu <-
        mkTestCPU "Transfer TYA - Negative"
        <&> setProgramMemory [0x98]
        .   setARegisterTest 0x1F
        .   setYRegisterTest 0xCA
    (_, cpu') <- runEmulatorTest execute cpu
    0xCA @=? aReg cpu'
    negFlag @=? fReg cpu'

transferTYAPositive :: TestTree
transferTYAPositive = testCase "Transfer TYA - Positive" $ do
    cpu <-
        mkTestCPU "Transfer TYA - Positive"
        <&> setProgramMemory [0x98]
        .   setARegisterTest 0x1F
        .   setYRegisterTest 0x64
    (_, cpu') <- runEmulatorTest execute cpu
    0x64 @=? aReg cpu'
    0x0 @=? fReg cpu'

transferTYAZero :: TestTree
transferTYAZero = testCase "Transfer TYA - Zero" $ do
    cpu <-
        mkTestCPU "Transfer TYA - Zero"
        <&> setProgramMemory [0x98]
        .   setARegisterTest 0x1F
        .   setYRegisterTest 0x0
    (_, cpu') <- runEmulatorTest execute cpu
    0x0 @=? aReg cpu'
    zeroFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}
