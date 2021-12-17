-- | Testing Stack Operations and Overflow/Underflow

module Test.StackTest
    ( stackOps
    ) where

import qualified Data.Array.IArray             as IA
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


stackOps :: TestTree
stackOps = testGroup
    "Stack Operations"
    [ transferTSX
    , transferTXS
    , pushAccumulator
    , pushFlags
    , popAccumulator
    , popFlags
    ]

{--------------------------------------  TSX Transfer Tests --------------------------------------}
transferTSX :: TestTree
transferTSX = testGroup
    "Stack Operations: SP to X"
    [transferTSXNegative, transferTSXPositive, transferTSXZero]

transferTSXNegative :: TestTree
transferTSXNegative = testCase "Transfer TSX - Negative" $ do
    cpu <-
        mkTestCPU "Transfer TSX - Negative"
        <&> setTestMemory 0xF000 [0xBA]
        .   setStackPointerTest 0x1CA
        .   setXRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0xCA @=? xReg cpu'
    negFlag @=? fReg cpu'

transferTSXPositive :: TestTree
transferTSXPositive = testCase "Transfer TSX - Positive" $ do
    cpu <-
        mkTestCPU "Transfer TSX - Positive"
        <&> setProgramMemory [0xBA]
        .   setStackPointerTest 0x164
        .   setXRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x64 @=? xReg cpu'
    0x0 @=? fReg cpu'

transferTSXZero :: TestTree
transferTSXZero = testCase "Transfer TSX - Zero" $ do
    cpu <-
        mkTestCPU "Transfer TSX - Zero"
        <&> setProgramMemory [0xBA]
        .   setStackPointerTest 0x100
        .   setXRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x0 @=? xReg cpu'
    zeroFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}

{--------------------------------------  TXS Transfer Tests --------------------------------------}
transferTXS :: TestTree
transferTXS = testGroup "Stack Operations: X to SP" [transferTXSTest]

transferTXSTest :: TestTree
transferTXSTest = testCase "Transfer TXS" $ do
    cpu <-
        mkTestCPU "Transfer TXS"
        <&> setProgramMemory [0x9A]
        .   setStackPointerTest 0x1C9
        .   setXRegisterTest 0x1F
    (_, cpu') <- runEmulatorTest execute cpu
    0x11F @=? sp cpu'

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PHA Transfer Tests --------------------------------------}
pushAccumulator :: TestTree
pushAccumulator =
    testGroup "Stack Operations: PHA" [stackPHA, stackPHAOverflow]

stackPHA :: TestTree
stackPHA = testCase "Push Accumulator" $ do
    cpu <-
        mkTestCPU "Push Accumulator"
        <&> setProgramMemory [0x48]
        .   setARegisterTest 0x5A
    (_, cpu') <- runEmulatorTest execute cpu
    let topStackVal = memory cpu' IA.! getSP (sp cpu)
    0x5A @=? topStackVal
    sp cpu @=? (sp cpu' + 1)

stackPHAOverflow :: TestTree
stackPHAOverflow = testCase "Push Accumulator - Overflow" $ do
    cpu <-
        mkTestCPU "Push Accumulator - Overflow"
        <&> setProgramMemory [0x48]
        .   setStackPointerTest 0x100
        .   setARegisterTest 0x5A
    let spPre = sp cpu
    (_, cpu') <- runEmulatorTest execute cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal
    0x1FF @=? sp cpu'

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PHP Transfer Tests --------------------------------------}
pushFlags :: TestTree
pushFlags = testGroup "Stack Operations: PHP" [stackPHP, stackPHPOverflow]

stackPHP :: TestTree
stackPHP = testCase "Push Flags" $ do
    cpu <- mkTestCPU "Push Flags" <&> setProgramMemory [0x08] . setFRegisterTest
        0x5A
    let spPre = sp cpu
    (_, cpu') <- runEmulatorTest execute cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal
    spPre @=? (sp cpu' + 1)

stackPHPOverflow :: TestTree
stackPHPOverflow = testCase "Push Flags - Overflow" $ do
    cpu <-
        mkTestCPU "Push Flags - Overflow"
        <&> setProgramMemory [0x08]
        .   setStackPointerTest 0x100
        .   setFRegisterTest 0x5A
    let spPre = sp cpu
    (_, cpu') <- runEmulatorTest execute cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PLP Transfer Tests --------------------------------------}
popFlags :: TestTree
popFlags = testGroup "Stack Operations: PLP" [stackPLP]

stackPLP :: TestTree
stackPLP = testCase "Pop Flags" $ do
    let testSP = 0x1FC
    cpu <-
        mkTestCPU "Pop Flags"
        <&> setProgramMemory [0x28]
        .   setTestMemory testSP [0x81]
        .   setStackPointerTest testSP
    (_, cpu') <- runEmulatorTest execute cpu
    sp cpu @=? (sp cpu' - 1)
    0x81 @=? fReg cpu'

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PLA Transfer Tests --------------------------------------}
popAccumulator :: TestTree
popAccumulator = testGroup
    "Stack Operations: PLA"
    [stackPLAPositive, stackPLANegative, stackPLAZero]

stackPLAPositive :: TestTree
stackPLAPositive = testCase "Pop Accumulator - Positive" $ do
    let testSP = 0x1FE
    cpu <-
        mkTestCPU "Pop Accumulator - Positive"
        <&> setProgramMemory [0x68]
        .   setTestMemory testSP [0x5D]
        .   setStackPointerTest testSP
        .   setARegisterTest 0x43
    (_, cpu') <- runEmulatorTest execute cpu
    sp cpu @=? (sp cpu' - 1)
    0x5D @=? aReg cpu'
    0x0 @=? fReg cpu'

stackPLANegative :: TestTree
stackPLANegative = testCase "Pop Accumulator - Negative" $ do
    let testSP = 0x1FE
    cpu <-
        mkTestCPU "Pop Accumulator - Negative"
        <&> setProgramMemory [0x68]
        .   setTestMemory testSP [0xDD]
        .   setStackPointerTest testSP
        .   setARegisterTest 0x43
    (_, cpu') <- runEmulatorTest execute cpu
    sp cpu @=? (sp cpu' - 1)
    0xDD @=? aReg cpu'
    negFlag @=? fReg cpu'

stackPLAZero :: TestTree
stackPLAZero = testCase "Pop Accumulator - Zero" $ do
    let testSP = 0x1FE
    cpu <-
        mkTestCPU "Pop Accumulator - Zero"
        <&> setProgramMemory [0x68]
        .   setTestMemory testSP [0x0]
        .   setStackPointerTest testSP
        .   setARegisterTest 0x43
    (_, cpu') <- runEmulatorTest execute cpu
    sp cpu @=? (sp cpu' - 1)
    0x0 @=? aReg cpu'
    zeroFlag @=? fReg cpu'

{-------------------------------------------------------------------------------------------------}
