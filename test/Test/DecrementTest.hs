-- | Decrement Operations Testing

module Test.DecrementTest
    ( decrement
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
import           Utils

decrement :: TestTree
decrement =
    testGroup "Decrement Operations" [decrementDEC, decrementDEX, decrementDEY]

{--------------------------------------  DEC Tests --------------------------------------}

decrementDEC :: TestTree
decrementDEC = testGroup
    "DEC Operations"
    [ decrementDECZeroPage
    , decrementDECZeroPageX
    , decrementDECAbsolute
    , decrementDECAbsoluteX
    ]

decrementDECZeroPage :: TestTree
decrementDECZeroPage = testCase "DEC - ZeroPage" $ do
    cpu            <- mkTestCPU "DECZeroPage" <&> setProgramMemory [0xC6, 0x57]
    (incVal, cpu') <- runEmulatorTest (execute >> fetchByte 0x57) cpu
    0xA7 @=? incVal
    negFlag @=? fReg cpu'

decrementDECZeroPageX :: TestTree
decrementDECZeroPageX = testCase "DEC - ZeroPage - X" $ do
    cpu <-
        mkTestCPU "DECZeroPageX"
        <&> setProgramMemory [0xD6, 0x10]
        .   setXRegisterTest 0xB1
    (incVal, cpu') <- runEmulatorTest (execute >> fetchByte 0xC1) cpu
    0x3D @=? incVal
    0x0 @=? fReg cpu'

decrementDECAbsolute :: TestTree
decrementDECAbsolute = testCase "DEC - Absolute" $ do
    cpu <-
        mkTestCPU "DECAbsolute"
        <&> setTestMemory 0x1A1A [0x57]
        .   setProgramMemory [0xCE, 0x1A, 0x1A]
    (incVal, cpu') <- runEmulatorTest (execute >> fetchByte 0x1A1A) cpu
    0x56 @=? incVal
    0x0 @=? fReg cpu'

decrementDECAbsoluteX :: TestTree
decrementDECAbsoluteX = testCase "DEC - Absolute - X" $ do
    cpu <-
        mkTestCPU "DECAbsoluteX"
        <&> setTestMemory 0x401F [0x1]
        .   setProgramMemory [0xDE, 0x0, 0x40]
        .   setXRegisterTest 0x1F
    (incVal, cpu') <- runEmulatorTest (execute >> fetchByte 0x401F) cpu
    0x0 @=? incVal
    zeroFlag @=? fReg cpu'

{--------------------------------------  DEX Tests --------------------------------------}
decrementDEX :: TestTree
decrementDEX = testGroup "DEX Operation" [decrementDEXImplied]

decrementDEXImplied :: TestTree
decrementDEXImplied = testCase "DEX - Implied" $ do
    cpu <- mkTestCPU "DEXImplied" <&> setProgramMemory [0xCA] . setXRegisterTest
        0x57
    (_, cpu') <- runEmulatorTest execute cpu
    0x56 @=? xReg cpu'
    0x0 @=? fReg cpu'

{--------------------------------------  DEY Tests --------------------------------------}
decrementDEY :: TestTree
decrementDEY = testGroup "DEY Operation" [decrementDEYImplied]

decrementDEYImplied :: TestTree
decrementDEYImplied = testCase "DEY - Implied" $ do
    cpu <- mkTestCPU "DEYImplied" <&> setProgramMemory [0x88] . setYRegisterTest
        0x57
    (_, cpu') <- runEmulatorTest execute cpu
    0x56 @=? yReg cpu'
    0x0 @=? fReg cpu'
