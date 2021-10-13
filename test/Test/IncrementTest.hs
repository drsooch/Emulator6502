-- | Increment Operations Testing

module IncrementTest
    ( increment
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

increment :: TestTree
increment =
    testGroup "Increment Operations" [incrementINC, incrementINX, incrementINY]

{--------------------------------------  INC Tests --------------------------------------}

incrementINC :: TestTree
incrementINC = testGroup
    "INC Operations"
    [ incrementINCZeroPage
    , incrementINCZeroPageX
    , incrementINCAbsolute
    , incrementINCAbsoluteX
    ]

incrementINCZeroPage :: TestTree
incrementINCZeroPage = testCase "INC - ZeroPage" $ do
    cpu            <- mkTestCPU "INCZeroPage" <&> setProgramMemory [0xE6, 0x57]
    (incVal, cpu') <- runEmulatorTest (execute >> fetchByte 0x57) cpu
    0xA9 @=? incVal
    negFlag @=? fReg cpu'

incrementINCZeroPageX :: TestTree
incrementINCZeroPageX = testCase "INC - ZeroPage - X" $ do
    cpu <-
        mkTestCPU "INCZeroPageX"
        <&> setProgramMemory [0xF6, 0x10]
        .   setXRegisterTest 0xB1
    (incVal, cpu') <- runEmulatorTest (execute >> fetchByte 0xC1) cpu
    0x3F @=? incVal
    0x0 @=? fReg cpu'

incrementINCAbsolute :: TestTree
incrementINCAbsolute = testCase "INC - Absolute" $ do
    cpu <-
        mkTestCPU "INCAbsolute"
        <&> setTestMemory 0x1A1A [0x57]
        .   setProgramMemory [0xEE, 0x1A, 0x1A]
    (incVal, cpu') <- runEmulatorTest (execute >> fetchByte 0x1A1A) cpu
    0x58 @=? incVal
    0x0 @=? fReg cpu'

incrementINCAbsoluteX :: TestTree
incrementINCAbsoluteX = testCase "INC - Absolute - X" $ do
    cpu <-
        mkTestCPU "INCAbsoluteX"
        <&> setTestMemory 0x401F [0xFF]
        .   setProgramMemory [0xFE, 0x0, 0x40]
        .   setXRegisterTest 0x1F
    (incVal, cpu') <- runEmulatorTest (execute >> fetchByte 0x401F) cpu
    0x0 @=? incVal
    zeroFlag @=? fReg cpu'

{--------------------------------------  INX Tests --------------------------------------}
incrementINX :: TestTree
incrementINX = testGroup "INX Operation" [incrementINXImplied]

incrementINXImplied :: TestTree
incrementINXImplied = testCase "INX - Implied" $ do
    cpu <- mkTestCPU "INXImplied" <&> setProgramMemory [0xE8] . setXRegisterTest
        0x57
    (_, cpu') <- runEmulatorTest execute cpu
    0x58 @=? xReg cpu'
    0x0 @=? fReg cpu'

{--------------------------------------  INY Tests --------------------------------------}
incrementINY :: TestTree
incrementINY = testGroup "INY Operation" [incrementINYImplied]

incrementINYImplied :: TestTree
incrementINYImplied = testCase "INY - Implied" $ do
    cpu <- mkTestCPU "INYImplied" <&> setProgramMemory [0xC8] . setYRegisterTest
        0x57
    (_, cpu') <- runEmulatorTest execute cpu
    0x58 @=? yReg cpu'
    0x0 @=? fReg cpu'
