-- | TAX, TAY, TXA, TYA

module TransferTest
    ( transfer
    ) where

import           Control.Monad.RWS.Strict       ( runRWST )
import           Emulator
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
    let cpu = mkTestCPU { aReg = 0xCA, xReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0xAA] >> execute) [] cpu
    0xCA @=? xReg cpu'
    negFlag @=? fReg cpu'

transferTAXPositive :: TestTree
transferTAXPositive = testCase "Transfer TAX - Positive" $ do
    let cpu = mkTestCPU { aReg = 0x64, xReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0xAA] >> execute) [] cpu
    0x64 @=? xReg cpu'
    0x0 @=? fReg cpu'

transferTAXZero :: TestTree
transferTAXZero = testCase "Transfer TAX - Zero" $ do
    let cpu = mkTestCPU { aReg = 0x0, xReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0xAA] >> execute) [] cpu
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
    let cpu = mkTestCPU { aReg = 0xCA, yReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0xA8] >> execute) [] cpu
    0xCA @=? yReg cpu'
    negFlag @=? fReg cpu'

transferTAYPositive :: TestTree
transferTAYPositive = testCase "Transfer TAY - Positive" $ do
    let cpu = mkTestCPU { aReg = 0x64, yReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0xA8] >> execute) [] cpu
    0x64 @=? yReg cpu'
    0x0 @=? fReg cpu'

transferTAYZero :: TestTree
transferTAYZero = testCase "Transfer TAY - Zero" $ do
    let cpu = mkTestCPU { aReg = 0x0, yReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0xA8] >> execute) [] cpu
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
    let cpu = mkTestCPU { xReg = 0xCA, aReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0x8A] >> execute) [] cpu
    0xCA @=? aReg cpu'
    negFlag @=? fReg cpu'

transferTXAPositive :: TestTree
transferTXAPositive = testCase "Transfer TXA - Positive" $ do
    let cpu = mkTestCPU { xReg = 0x64, aReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0x8A] >> execute) [] cpu
    0x64 @=? aReg cpu'
    0x0 @=? fReg cpu'

transferTXAZero :: TestTree
transferTXAZero = testCase "Transfer TXA - Zero" $ do
    let cpu = mkTestCPU { xReg = 0x0, aReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0x8A] >> execute) [] cpu
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
    let cpu = mkTestCPU { yReg = 0xCA, aReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0x98] >> execute) [] cpu
    0xCA @=? aReg cpu'
    negFlag @=? fReg cpu'

transferTYAPositive :: TestTree
transferTYAPositive = testCase "Transfer TYA - Positive" $ do
    let cpu = mkTestCPU { yReg = 0x64, aReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0x98] >> execute) [] cpu
    0x64 @=? aReg cpu'
    0x0 @=? fReg cpu'

transferTYAZero :: TestTree
transferTYAZero = testCase "Transfer TYA - Zero" $ do
    let cpu = mkTestCPU { yReg = 0x0, aReg = 0x1F }
    (_, cpu', _) <- runRWST (setMemory 0xF000 [0x98] >> execute) [] cpu
    0x0 @=? aReg cpu'
    zeroFlag @=? fReg cpu'
{-------------------------------------------------------------------------------------------------}
