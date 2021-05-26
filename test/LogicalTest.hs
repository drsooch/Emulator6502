-- | Logical Operations Testing

module LogicalTest
    ( logical
    ) where

import           Control.Monad.RWS.Strict       ( runRWST )
import           Execution                      ( execute )
import           Flags                          ( negFlag
                                                , zeroFlag
                                                )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , testCase
                                                )
import           TestUtils                      ( mkTestCPU
                                                , setProgramMemory
                                                )
import           Types                          ( CPU(..)
                                                , CPUState(Running)
                                                , setMemory
                                                )

logical :: TestTree
logical = testGroup "Logical Operations" [logicalAnd, logicalXor]

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
    let cpu = setProgramMemory [0x29, 0x57] mkTestCPU { aReg = 0x1F }
    (result, cpu', _) <- runRWST execute [] cpu
    Running @=? result
    0x17 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndZeroPage :: TestTree
logicalAndZeroPage = testCase "Logical And - ZeroPage" $ do
    let cpu = setProgramMemory [0x25, 0x57] mkTestCPU { aReg = 0x1F }
    (result, cpu', _) <- runRWST execute [] cpu
    Running @=? result
    0x8 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndZeroPageX :: TestTree
logicalAndZeroPageX = testCase "Logical And - ZeroPage - X" $ do
    let cpu =
            setProgramMemory [0x35, 0x1A] mkTestCPU { aReg = 0x1F, xReg = 0xD5 }
    (result, cpu', _) <- runRWST execute [] cpu
    Running @=? result
    0x10 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndAbsolute :: TestTree
logicalAndAbsolute = testCase "Logical And - Absolute" $ do
    let cpu = setProgramMemory [0x2D, 0x0, 0x40] mkTestCPU { aReg = 0x1F }
    (result, cpu', _) <- runRWST (setMemory 0x4000 [0x4A] >> execute) [] cpu
    Running @=? result
    0xA @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndAbsoluteX :: TestTree
logicalAndAbsoluteX = testCase "Logical And - Absolute - X" $ do
    let
        cpu = setProgramMemory [0x3D, 0x0, 0x40]
                               mkTestCPU { aReg = 0x1F, xReg = 0xAA }
    (result, cpu', _) <- runRWST (setMemory 0x40AA [0x4A] >> execute) [] cpu
    Running @=? result
    0xA @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndAbsoluteY :: TestTree
logicalAndAbsoluteY = testCase "Logical And - Absolute - Y" $ do
    let
        cpu = setProgramMemory [0x39, 0x0, 0x40]
                               mkTestCPU { aReg = 0xB2, yReg = 0xC1 }
    (result, cpu', _) <- runRWST (setMemory 0x40C1 [0x5F] >> execute) [] cpu
    Running @=? result
    0x12 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndIndirectX :: TestTree
logicalAndIndirectX = testCase "Logical And - Indirect - X" $ do
    let cpu =
            setProgramMemory [0x21, 0x0] mkTestCPU { aReg = 0xC4, xReg = 0xBB }
    (result, cpu', _) <- runRWST (setMemory 0x4344 [0xC] >> execute) [] cpu
    Running @=? result
    0x04 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndIndirectY :: TestTree
logicalAndIndirectY = testCase "Logical And - Indirect - Y" $ do
    let cpu =
            setProgramMemory [0x31, 0x57] mkTestCPU { aReg = 0xD1, yReg = 0xBB }
    (result, cpu', _) <- runRWST (setMemory 0xA863 [0xFF] >> execute) [] cpu
    Running @=? result
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
    let cpu = setProgramMemory [0x49, 0xB7] mkTestCPU { aReg = 0xBC }
    (result, cpu', _) <- runRWST execute [] cpu
    Running @=? result
    0xB @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalXorImmediateZeroFlag :: TestTree
logicalXorImmediateZeroFlag =
    testCase "Logical XOR - Immediate - Zero Flag" $ do
        let cpu = setProgramMemory [0x49, 0x00] mkTestCPU { aReg = 0x00 }
        (result, cpu', _) <- runRWST execute [] cpu
        Running @=? result
        0x0 @=? aReg cpu'
        zeroFlag @=? fReg cpu'

logicalXorZeroPage :: TestTree
logicalXorZeroPage = testCase "Logical XOR - ZeroPage" $ do
    let cpu = setProgramMemory [0x45, 0x57] mkTestCPU { aReg = 0x1F }
    (result, cpu', _) <- runRWST execute [] cpu
    Running @=? result
    0xB7 @=? aReg cpu'
    negFlag @=? fReg cpu'

logicalXorZeroPageX :: TestTree
logicalXorZeroPageX = testCase "Logical XOR - ZeroPage X" $ do
    let cpu =
            setProgramMemory [0x55, 0x57] mkTestCPU { aReg = 0xCC, xReg = 0x4A }
    (result, cpu', _) <- runRWST execute [] cpu
    Running @=? result
    0x92 @=? aReg cpu'
    negFlag @=? fReg cpu'

logicalXorAbsolute :: TestTree
logicalXorAbsolute = testCase "Logical XOR - Absolute" $ do
    let cpu = setProgramMemory [0x4D, 0xEE, 0x5F] mkTestCPU { aReg = 0x05 }
    (result, cpu', _) <- runRWST (setMemory 0x5FEE [0xEA] >> execute) [] cpu
    Running @=? result
    0xEF @=? aReg cpu'
    negFlag @=? fReg cpu'

logicalXorAbsoluteX :: TestTree
logicalXorAbsoluteX = testCase "Logical XOR - Absolute X" $ do
    let cpu = setProgramMemory [0x5D, 0x00, 0x30]
                               mkTestCPU { aReg = 0x2A, xReg = 0x92 }
    (result, cpu', _) <- runRWST (setMemory 0x3092 [0x7B] >> execute) [] cpu
    Running @=? result
    0x51 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalXorAbsoluteY :: TestTree
logicalXorAbsoluteY = testCase "Logical XOR - Absolute Y" $ do
    let cpu = setProgramMemory [0x59, 0x00, 0x40]
                               mkTestCPU { aReg = 0x2A, yReg = 0xF2 }
    (result, cpu', _) <- runRWST (setMemory 0x40F2 [0x7B] >> execute) [] cpu
    Running @=? result
    0x51 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalXorIndirectX :: TestTree
logicalXorIndirectX = testCase "Logical XOR - Indirect X" $ do
    let cpu =
            setProgramMemory [0x41, 0x32] mkTestCPU { aReg = 0xCC, xReg = 0xF2 }
    (result, cpu', _) <- runRWST (setMemory 0xDADB [0xDB] >> execute) [] cpu
    Running @=? result
    0x17 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalXorIndirectY :: TestTree
logicalXorIndirectY = testCase "Logical XOR - Indirect Y" $ do
    let cpu =
            setProgramMemory [0x51, 0x32] mkTestCPU { aReg = 0x11, yReg = 0x40 }
    (result, cpu', _) <- runRWST (setMemory 0xCD0D [0xB1] >> execute) [] cpu
    Running @=? result
    0xA0 @=? aReg cpu'
    negFlag @=? fReg cpu'
