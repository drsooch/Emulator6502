-- | Testing Stack Operations and Overflow/Underflow

module StackTest
    ( stackOps
    ) where

import           Control.Monad.RWS.Strict       ( runRWST )
import qualified Data.Array.IArray             as IA
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
import           TestUtils                      ( mkTestCPU )
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
    let cpu = mkTestCPU { sp = 0x1CA, xReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0xBA] >> execute) [] cpu
    0xCA @=? xReg cpu'
    negFlag @=? fReg cpu'

transferTSXPositive :: TestTree
transferTSXPositive = testCase "Transfer TSX - Positive" $ do
    let cpu = mkTestCPU { sp = 0x164, xReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0xBA] >> execute) [] cpu
    0x64 @=? xReg cpu'
    0x0 @=? fReg cpu'

transferTSXZero :: TestTree
transferTSXZero = testCase "Transfer TSX - Zero" $ do
    let cpu = mkTestCPU { sp = 0x100, xReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0xBA] >> execute) [] cpu
    0x0 @=? xReg cpu'
    zeroFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}

{--------------------------------------  TXS Transfer Tests --------------------------------------}
transferTXS :: TestTree
transferTXS = testGroup "Stack Operations: X to SP" [transferTXSTest]

transferTXSTest :: TestTree
transferTXSTest = testCase "Transfer TXS" $ do
    let cpu = mkTestCPU { sp = 0x1C9, xReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0x9A] >> execute) [] cpu
    0x11F @=? sp cpu'

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PHA Transfer Tests --------------------------------------}
pushAccumulator :: TestTree
pushAccumulator =
    testGroup "Stack Operations: PHA" [stackPHA, stackPHAOverflow]

stackPHA :: TestTree
stackPHA = testCase "Push Accumulator" $ do
    let cpu   = mkTestCPU { aReg = 0x5A }
    let spPre = sp cpu
    (cpuState, cpu', _) <- runRWST (setMemory 0xF000 [0x48] >> execute) [] cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal
    spPre @=? (sp cpu' + 1)
    Running @=? cpuState

stackPHAOverflow :: TestTree
stackPHAOverflow = testCase "Push Accumulator - Overflow" $ do
    let cpu = mkTestCPU { sp = 0x100, aReg = 0x5A }
    let spPre = sp cpu
    (cpuState, cpu', _) <- runRWST (setMemory 0xF000 [0x48] >> execute) [] cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal
    CPUError StackOverflow @=? cpuState

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PHP Transfer Tests --------------------------------------}
pushFlags :: TestTree
pushFlags = testGroup "Stack Operations: PHP" [stackPHP, stackPHPOverflow]

stackPHP :: TestTree
stackPHP = testCase "Push Flags" $ do
    let cpu   = mkTestCPU { fReg = 0x5A }
    let spPre = sp cpu
    (cpuState, cpu', _) <- runRWST (setMemory 0xF000 [0x08] >> execute) [] cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal
    spPre @=? (sp cpu' + 1)
    Running @=? cpuState

stackPHPOverflow :: TestTree
stackPHPOverflow = testCase "Push Flags - Overflow" $ do
    let cpu = mkTestCPU { sp = 0x100, fReg = 0x5A }
    let spPre = sp cpu
    (cpuState, cpu', _) <- runRWST (setMemory 0xF000 [0x08] >> execute) [] cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal
    CPUError StackOverflow @=? cpuState

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PLP Transfer Tests --------------------------------------}
popFlags :: TestTree
popFlags = testGroup "Stack Operations: PLP" [stackPLP, stackPLPUnderflow]

stackPLP :: TestTree
stackPLP = testCase "Pop Flags" $ do
    let cpu   = mkTestCPU { sp = 0x01FC }
    let cpu' = cpu { memory = memory cpu IA.// [(getSP $ sp cpu, 0x81)] }
    let spPre = sp cpu
    (cpuState, cpu'', _) <- runRWST (setMemory 0xF000 [0x28] >> execute) [] cpu'
    spPre @=? (sp cpu'' - 1)
    0x81 @=? fReg cpu''
    Running @=? cpuState

stackPLPUnderflow :: TestTree
stackPLPUnderflow = testCase "Push Flags - Underflow" $ do
    let cpu   = mkTestCPU
    let cpu' = cpu { memory = memory cpu IA.// [(getSP $ sp cpu, 0x81)] }
    let spPre = sp cpu
    (cpuState, cpu'', _) <- runRWST (setMemory 0xF000 [0x28] >> execute) [] cpu'
    spPre @=? (sp cpu'' - 1)
    0x81 @=? fReg cpu''
    CPUError StackUnderflow @=? cpuState

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PLP Transfer Tests --------------------------------------}
popAccumulator :: TestTree
popAccumulator = testGroup
    "Stack Operations: PLA"
    [stackPLAPositive, stackPLANegative, stackPLAZero, stackPLAUnderflow]

stackPLAPositive :: TestTree
stackPLAPositive = testCase "Pop Accumulator - Positive" $ do
    let cpu = mkTestCPU { sp = 0x01FC }
    let
        cpu' = cpu { memory = memory cpu IA.// [(getSP $ sp cpu, 0x5D)]
                   , aReg   = 0x43
                   }
    let spPre = sp cpu
    (cpuState, cpu'', _) <- runRWST (setMemory 0xF000 [0x68] >> execute) [] cpu'
    spPre @=? (sp cpu'' - 1)
    0x5D @=? aReg cpu''
    0x0 @=? fReg cpu''
    Running @=? cpuState

stackPLANegative :: TestTree
stackPLANegative = testCase "Pop Accumulator - Negative" $ do
    let cpu = mkTestCPU { sp = 0x01FC }
    let
        cpu' = cpu { memory = memory cpu IA.// [(getSP $ sp cpu, 0xDD)]
                   , aReg   = 0x43
                   }
    let spPre = sp cpu
    (cpuState, cpu'', _) <- runRWST (setMemory 0xF000 [0x68] >> execute) [] cpu'
    spPre @=? (sp cpu'' - 1)
    0xDD @=? aReg cpu''
    negFlag @=? fReg cpu''
    Running @=? cpuState

stackPLAZero :: TestTree
stackPLAZero = testCase "Pop Accumulator - Zero" $ do
    let cpu = mkTestCPU { sp = 0x01FC }
    let
        cpu' = cpu { memory = memory cpu IA.// [(getSP $ sp cpu, 0x0)]
                   , aReg   = 0x43
                   }
    let spPre = sp cpu
    (cpuState, cpu'', _) <- runRWST (setMemory 0xF000 [0x68] >> execute) [] cpu'
    spPre @=? (sp cpu'' - 1)
    0x0 @=? aReg cpu''
    zeroFlag @=? fReg cpu''
    Running @=? cpuState


stackPLAUnderflow :: TestTree
stackPLAUnderflow = testCase "Pop Accumulator - Overflow" $ do
    let cpu = mkTestCPU
    let
        cpu' = cpu { memory = memory cpu IA.// [(getSP $ sp cpu, 0x69)]
                   , aReg   = 0xA0
                   }
    let spPre = sp cpu
    (cpuState, cpu'', _) <- runRWST (setMemory 0xF000 [0x68] >> execute) [] cpu'
    spPre @=? (sp cpu'' - 1)
    0x69 @=? aReg cpu''
    CPUError StackUnderflow @=? cpuState

{-------------------------------------------------------------------------------------------------}
