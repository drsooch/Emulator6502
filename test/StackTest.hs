-- | Testing Stack Operations and Overflow/Underflow

module StackTest
    ( stackOps
    ) where

import           Control.Monad.State.Strict     ( runStateT )
import qualified Data.Array.IArray             as IA
import           Execution
import           Flags
import           Memory
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
    cpu <- mkTestCPU "Transfer TSX - Negative"
        >>= \c -> return c { sp = 0x1CA, xReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0xBA] >> execute) cpu
    0xCA @=? xReg cpu'
    negFlag @=? fReg cpu'

transferTSXPositive :: TestTree
transferTSXPositive = testCase "Transfer TSX - Positive" $ do
    cpu <- mkTestCPU "Transfer TSX - Positive"
        >>= \c -> return c { sp = 0x164, xReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0xBA] >> execute) cpu
    0x64 @=? xReg cpu'
    0x0 @=? fReg cpu'

transferTSXZero :: TestTree
transferTSXZero = testCase "Transfer TSX - Zero" $ do
    cpu <- mkTestCPU "Transfer TSX - Zero"
        >>= \c -> return c { sp = 0x100, xReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0xBA] >> execute) cpu
    0x0 @=? xReg cpu'
    zeroFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}

{--------------------------------------  TXS Transfer Tests --------------------------------------}
transferTXS :: TestTree
transferTXS = testGroup "Stack Operations: X to SP" [transferTXSTest]

transferTXSTest :: TestTree
transferTXSTest = testCase "Transfer TXS" $ do
    cpu <- mkTestCPU "Transfer TXS"
        >>= \c -> return c { sp = 0x1C9, xReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x9A] >> execute) cpu
    0x11F @=? sp cpu'

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PHA Transfer Tests --------------------------------------}
pushAccumulator :: TestTree
pushAccumulator =
    testGroup "Stack Operations: PHA" [stackPHA, stackPHAOverflow]

stackPHA :: TestTree
stackPHA = testCase "Push Accumulator" $ do
    cpu <- mkTestCPU "Push Accumulator" >>= \c -> return c { aReg = 0x5A }
    let spPre = sp cpu
    (_, cpu') <- runStateT (setMemory 0xF000 [0x48] >> execute) cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal
    spPre @=? (sp cpu' + 1)

stackPHAOverflow :: TestTree
stackPHAOverflow = testCase "Push Accumulator - Overflow" $ do
    cpu <- mkTestCPU "Push Accumulator - Overflow"
        >>= \c -> return c { sp = 0x100, aReg = 0x5A }
    let spPre = sp cpu
    (_, cpu') <- runStateT (setMemory 0xF000 [0x48] >> execute) cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal
    0x1FF @=? sp cpu'

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PHP Transfer Tests --------------------------------------}
pushFlags :: TestTree
pushFlags = testGroup "Stack Operations: PHP" [stackPHP, stackPHPOverflow]

stackPHP :: TestTree
stackPHP = testCase "Push Flags" $ do
    cpu <- mkTestCPU "Push Flags" >>= \c -> return c { fReg = 0x5A }
    let spPre = sp cpu
    (_, cpu') <- runStateT (setMemory 0xF000 [0x08] >> execute) cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal
    spPre @=? (sp cpu' + 1)

stackPHPOverflow :: TestTree
stackPHPOverflow = testCase "Push Flags - Overflow" $ do
    cpu <- mkTestCPU "Push Flags - Overflow"
        >>= \c -> return c { sp = 0x100, fReg = 0x5A }
    let spPre = sp cpu
    (_, cpu') <- runStateT (setMemory 0xF000 [0x08] >> execute) cpu
    let topStackVal = memory cpu' IA.! getSP spPre
    0x5A @=? topStackVal

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PLP Transfer Tests --------------------------------------}
popFlags :: TestTree
popFlags = testGroup "Stack Operations: PLP" [stackPLP]

stackPLP :: TestTree
stackPLP = testCase "Pop Flags" $ do
    cpu <- mkTestCPU "Pop Flags" >>= \c -> return c { sp = 0x01FC }
    let cpu' = cpu { memory = memory cpu IA.// [(getSP $ sp cpu, 0x81)] }
    let spPre = sp cpu
    (_, cpu'') <- runStateT (setMemory 0xF000 [0x28] >> execute) cpu'
    spPre @=? (sp cpu'' - 1)
    0x81 @=? fReg cpu''

{-------------------------------------------------------------------------------------------------}

{--------------------------------------  PLA Transfer Tests --------------------------------------}
popAccumulator :: TestTree
popAccumulator = testGroup
    "Stack Operations: PLA"
    [stackPLAPositive, stackPLANegative, stackPLAZero]

stackPLAPositive :: TestTree
stackPLAPositive = testCase "Pop Accumulator - Positive" $ do
    cpu <- mkTestCPU "Pop Accumulator - Positive" >>= \c -> return c
        { memory = memory c IA.// [(0x1FE, 0x5D)]
        , aReg   = 0x43
        , sp     = 0x1FE
        }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x68] >> execute) cpu
    sp cpu @=? (sp cpu' - 1)
    0x5D @=? aReg cpu'
    0x0 @=? fReg cpu'

stackPLANegative :: TestTree
stackPLANegative = testCase "Pop Accumulator - Negative" $ do
    cpu <- mkTestCPU "Pop Accumulator - Negative" >>= \c -> return c
        { memory = memory c IA.// [(0x1FE, 0xDD)]
        , aReg   = 0x43
        , sp     = 0x1FE
        }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x68] >> execute) cpu
    sp cpu @=? (sp cpu' - 1)
    0xDD @=? aReg cpu'
    negFlag @=? fReg cpu'

stackPLAZero :: TestTree
stackPLAZero = testCase "Pop Accumulator - Zero" $ do
    cpu <- mkTestCPU "Pop Accumulator - Zero" >>= \c -> return c
        { memory = memory c IA.// [(0x1FE, 0x0)]
        , aReg   = 0x43
        , sp     = 0x1FE
        }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x68] >> execute) cpu
    sp cpu @=? (sp cpu' - 1)
    0x0 @=? aReg cpu'
    zeroFlag @=? fReg cpu'

{-------------------------------------------------------------------------------------------------}
