-- | Logical Operations Testing

module LogicalTest
    ( logical
    ) where

import           Control.Monad.RWS.Strict       ( runRWST )
import           Emulator                       ( CPU(..)
                                                , CPUState(Running)
                                                , setMemory
                                                )
import           Execution                      ( execute )
import           Flags                          ( zeroFlag )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , testCase
                                                )
import           TestUtils                      ( mkTestCPU
                                                , setProgramMemory
                                                )

logical :: TestTree
logical = testGroup "Logical Operations" [logicalAnd]

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
    0x17 @=? aReg cpu'
    0x0 @=? fReg cpu'

logicalAndZeroPageX :: TestTree
logicalAndZeroPageX = testCase "Logical And - ZeroPage - X" $ do
    let cpu =
            setProgramMemory [0x35, 0x1A] mkTestCPU { aReg = 0x1F, xReg = 0xD5 }
    (result, cpu', _) <- runRWST execute [] cpu
    Running @=? result
    0xF @=? aReg cpu'
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
            setProgramMemory [0x21, 0x0] mkTestCPU { aReg = 0xAA, xReg = 0xBB }
    (result, cpu', _) <- runRWST execute [] cpu
    Running @=? result
    0x88 @=? aReg cpu'
    zeroFlag @=? fReg cpu'

logicalAndIndirectY :: TestTree
logicalAndIndirectY = testCase "Logical And - Indirect - Y" $ do
    let cpu =
            setProgramMemory [0x31, 0xFF] mkTestCPU { aReg = 0xD1, yReg = 0xBB }
    (result, cpu', _) <- runRWST (setMemory 0x1BA [0x1E] >> execute) [] cpu
    Running @=? result
    0x10 @=? aReg cpu'
    0x0 @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}
