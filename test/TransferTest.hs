-- | TAX, TAY, TXA, TYA

module TransferTest
    ( transfer
    ) where

import           Control.Monad.State.Strict     ( runStateT )
import           Execution
import           Flags
import           Memory
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
    cpu <- mkTestCPU "Transfer TAX - Negative"
        >>= \c -> return c { aReg = 0xCA, xReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0xAA] >> execute) cpu
    0xCA @=? xReg cpu'
    negFlag @=? fReg cpu'

transferTAXPositive :: TestTree
transferTAXPositive = testCase "Transfer TAX - Positive" $ do
    cpu <- mkTestCPU "Transfer TAX - Positive"
        >>= \c -> return c { aReg = 0x64, xReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0xAA] >> execute) cpu
    0x64 @=? xReg cpu'
    0x0 @=? fReg cpu'

transferTAXZero :: TestTree
transferTAXZero = testCase "Transfer TAX - Zero" $ do
    cpu <- mkTestCPU "Transfer TAX - Zero"
        >>= \c -> return c { aReg = 0x0, xReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0xAA] >> execute) cpu
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
    cpu <- mkTestCPU "Transfer TAY - Negative"
        >>= \c -> return c { aReg = 0xCA, yReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0xA8] >> execute) cpu
    0xCA @=? yReg cpu'
    negFlag @=? fReg cpu'

transferTAYPositive :: TestTree
transferTAYPositive = testCase "Transfer TAY - Positive" $ do
    cpu <- mkTestCPU "Transfer TAY - Positive"
        >>= \c -> return c { aReg = 0x64, yReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0xA8] >> execute) cpu
    0x64 @=? yReg cpu'
    0x0 @=? fReg cpu'

transferTAYZero :: TestTree
transferTAYZero = testCase "Transfer TAY - Zero" $ do
    cpu <- mkTestCPU "Transfer TAY - Zero"
        >>= \c -> return c { aReg = 0x0, yReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0xA8] >> execute) cpu
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
    cpu <- mkTestCPU "Transfer TXA - Negative"
        >>= \c -> return c { xReg = 0xCA, aReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x8A] >> execute) cpu
    0xCA @=? aReg cpu'
    negFlag @=? fReg cpu'

transferTXAPositive :: TestTree
transferTXAPositive = testCase "Transfer TXA - Positive" $ do
    cpu <- mkTestCPU "Transfer TXA - Positive"
        >>= \c -> return c { xReg = 0x64, aReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x8A] >> execute) cpu
    0x64 @=? aReg cpu'
    0x0 @=? fReg cpu'

transferTXAZero :: TestTree
transferTXAZero = testCase "Transfer TXA - Zero" $ do
    cpu <- mkTestCPU "Transfer TXA - Zero"
        >>= \c -> return c { xReg = 0x0, aReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x8A] >> execute) cpu
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
    cpu <- mkTestCPU "Transfer TYA - Negative"
        >>= \c -> return c { yReg = 0xCA, aReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x98] >> execute) cpu
    0xCA @=? aReg cpu'
    negFlag @=? fReg cpu'

transferTYAPositive :: TestTree
transferTYAPositive = testCase "Transfer TYA - Positive" $ do
    cpu <- mkTestCPU "Transfer TYA - Positive"
        >>= \c -> return c { yReg = 0x64, aReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x98] >> execute) cpu
    0x64 @=? aReg cpu'
    0x0 @=? fReg cpu'

transferTYAZero :: TestTree
transferTYAZero = testCase "Transfer TYA - Zero" $ do
    cpu <- mkTestCPU "Transfer TYA - Zero"
        >>= \c -> return c { yReg = 0x0, aReg = 0x1F }
    (_, cpu') <- runStateT (setMemory 0xF000 [0x98] >> execute) cpu
    0x0 @=? aReg cpu'
    zeroFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}
