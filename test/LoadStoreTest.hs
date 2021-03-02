module LoadStoreTest
    ( loadStore
    ) where

import           Control.Monad.RWS.Strict       ( gets
                                                , runRWST
                                                )
import           Data.Array.IArray              ( (//)
                                                , listArray
                                                )
import           Data.Bits                      ( Bits((.&.), (.|.)) )
import           Emulator                       ( Byte
                                                , CPU(..)
                                                , Emulator
                                                , Flags(..)
                                                , Memory
                                                , ProgramCounter(PC)
                                                , Register(Reg)
                                                , StackPointer(SP)
                                                , setMemory
                                                )
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

-- Testing CPU with default values initialized
mkTestCPU :: CPU
mkTestCPU = CPU { .. }
  where
    mem    = listArray (0, 0xFFFF) (repeat 0) -- Array of 64kb init to 0
    memory = setTestZeroPage mem
    pc     = PC 0xF000
    sp     = SP 0x01FF
    xReg   = Reg 0x0
    yReg   = Reg 0x0
    aReg   = Reg 0x0
    fReg   = Flags 0x0

-- Set the ZeroPage to have useful information
setTestZeroPage :: Memory -> Memory
setTestZeroPage mem =
    let zp = [ (ix, fromIntegral ix) | ix <- [0 .. 0xFF] ] in mem // zp

loadStore :: TestTree
loadStore = testGroup
    "LoadStore"
    [loadARegTestGroup, loadXRegTestGroup, loadYRegTestGroup]

loadYRegTestGroup :: TestTree
loadYRegTestGroup =
    testGroup "Loading Y Register"
        $  [loadYRegImmediate, loadYRegZeroPage, loadYRegZeroPageX]
        <> [loadYRegAbsolute, loadYRegAbsoluteX]

loadXRegTestGroup :: TestTree
loadXRegTestGroup =
    testGroup "Loading X Register"
        $  [loadXRegImmediate, loadXRegZeroPage, loadXRegZeroPageY]
        <> [loadXRegAbsolute, loadXRegAbsoluteY]

loadARegTestGroup :: TestTree
loadARegTestGroup =
    testGroup "Loading A Register"
        $  [loadARegImmediatePos, loadARegImmediateNeg, loadARegImmediateZero]
        <> [loadARegZeroPage, loadARegZeroPageXNoWrap, loadARegZeroPageXWrap]
        <> [loadARegAbsolute, loadARegAbsoluteX, loadARegAbsoluteY]
        <> [loadARegIndirectX, loadARegIndirectY]

-- Execute a single Load instruction and check for proper output
loadReg
    :: (CPU -> Register) -- ^ Which register we are testing
    -> [Byte]            -- ^ Bytes to insert into memory
    -> Byte              -- ^  Expected Result in Register
    -> Flags             -- ^ Expected Flag Bits
    -> CPU               -- ^  CPU with setup data
    -> String            -- ^ Test Name
    -> TestTree
loadReg reg bytes expect flag cpu name = testCase name $ do
    ((aReg, flags), _, _) <- runRWST (loadReg' reg bytes) [] cpu
    Reg expect @=? aReg -- check A Register
    flag @=? flags .&. (zeroFlag .|. negFlag) -- Check Flag

loadReg' :: (CPU -> Register) -> [Byte] -> Emulator (Register, Flags)
loadReg' reg bytes =
    setMemory 0xF000 bytes >> execute >> gets (\c -> (reg c, fReg c))

{-------------------------------------- A Register Tests --------------------------------------}
loadAReg :: [Byte] -> Byte -> Flags -> CPU -> String -> TestTree
loadAReg = loadReg aReg

-- NOTE: We test the flag bits explicitly using LDA Immediate
-- The rest of the addressing modes only deal with special cases (Page boundary) not flags
loadARegImmediatePos :: TestTree
loadARegImmediatePos = loadAReg [0xA9, 0x39]
                                0x39
                                0x0
                                mkTestCPU
                                "Load A Register - Immediate - Positive"

loadARegImmediateNeg :: TestTree
loadARegImmediateNeg = loadAReg [0xA9, 0x9D]
                                0x9D
                                negFlag
                                mkTestCPU
                                "Load A Register - Immediate - Negative"

loadARegImmediateZero :: TestTree
loadARegImmediateZero = loadAReg [0xA9, 0x0]
                                 0x0
                                 zeroFlag
                                 mkTestCPU
                                 "Load A Register - Immediate - Zero"

loadARegZeroPage :: TestTree
loadARegZeroPage =
    loadAReg [0xA5, 0x27] 0x27 0x0 mkTestCPU "Load A Register - ZeroPage"

loadARegZeroPageXNoWrap :: TestTree
loadARegZeroPageXNoWrap = loadAReg [0xB5, 0x34]
                                   0x49
                                   0
                                   cpu
                                   "Load A Register - ZeroPageX - No Wrap"
    where cpu = mkTestCPU { xReg = Reg 0x15 }

loadARegZeroPageXWrap :: TestTree
loadARegZeroPageXWrap = loadAReg [0xB5, 0xD9]
                                 0x30
                                 0
                                 cpu
                                 "Load A Register - ZeroPageX - Wrap"
    where cpu = mkTestCPU { xReg = Reg 0x57 }

loadARegAbsolute :: TestTree
loadARegAbsolute = loadAReg [0xAD, 0x34, 0x12]
                            0x57
                            0
                            cpu'
                            "Load A Register - Absolute"
  where
    cpu  = mkTestCPU
    mem  = memory cpu // [(0x1234, 0x57)]
    cpu' = cpu { memory = mem }

loadARegAbsoluteX :: TestTree
loadARegAbsoluteX = loadAReg [0xBD, 0x34, 0x12]
                             0x57
                             0
                             cpu'
                             "Load A Register - AbsoluteX"
  where
    cpu  = mkTestCPU { xReg = Reg 0xF4 }
    mem  = memory cpu // [(0x1328, 0x57)]
    cpu' = cpu { memory = mem }

loadARegAbsoluteY :: TestTree
loadARegAbsoluteY = loadAReg [0xB9, 0x34, 0x12]
                             0x57
                             0
                             cpu'
                             "Load A Register - AbsoluteY"
  where
    cpu  = mkTestCPU { yReg = Reg 0xF4 }
    mem  = memory cpu // [(0x1328, 0x57)]
    cpu' = cpu { memory = mem }

loadARegIndirectX :: TestTree
loadARegIndirectX = loadAReg [0xA1, 0xC9]
                             0xFF
                             negFlag
                             cpu'
                             "Load A Register - IndirectX"
  where
    cpu  = mkTestCPU { xReg = Reg 0xC0 }
    mem  = memory cpu // [(0x8A89, 0xFF)]
    cpu' = cpu { memory = mem }

loadARegIndirectY :: TestTree
loadARegIndirectY = loadAReg [0xB1, 0xC9]
                             0xFF
                             negFlag
                             cpu'
                             "Load A Register - IndirectY"
  where
    cpu  = mkTestCPU { yReg = Reg 0xBB }
    mem  = memory cpu // [(0xCB84, 0xFF)]
    cpu' = cpu { memory = mem }

{-------------------------------------- X Register Tests --------------------------------------}
loadXReg :: [Byte] -> Byte -> Flags -> CPU -> String -> TestTree
loadXReg = loadReg xReg

loadXRegImmediate :: TestTree
loadXRegImmediate =
    loadXReg [0xA2, 0x39] 0x39 0x0 mkTestCPU "Load X Register - Immediate"

loadXRegZeroPage :: TestTree
loadXRegZeroPage =
    loadXReg [0xA6, 0x0] 0x0 zeroFlag mkTestCPU "Load X Register - ZeroPage"

loadXRegZeroPageY :: TestTree
loadXRegZeroPageY = loadXReg [0xB6, 0x7F]
                             0x8F
                             negFlag
                             cpu
                             "Load X Register - ZeroPage - Y"
    where cpu = mkTestCPU { yReg = Reg 0x10 }

loadXRegAbsolute :: TestTree
loadXRegAbsolute = loadXReg [0xAE, 0x00, 0xF0]
                            0xAE
                            negFlag
                            mkTestCPU
                            "Load X Register - Absolute"

loadXRegAbsoluteY :: TestTree
loadXRegAbsoluteY = loadXReg [0xBE, 0x00, 0x30]
                             0x21
                             0x0
                             cpu'
                             "Load X Register - Absolute - Y"
  where
    cpu  = mkTestCPU { yReg = Reg 0xAA }
    mem  = memory cpu // [(0x30AA, 0x21)]
    cpu' = cpu { memory = mem }

{-------------------------------------- Y Register Tests --------------------------------------}
loadYReg :: [Byte] -> Byte -> Flags -> CPU -> String -> TestTree
loadYReg = loadReg yReg

loadYRegImmediate :: TestTree
loadYRegImmediate =
    loadYReg [0xA0, 0x41] 0x41 0x0 mkTestCPU "Load Y Register - Immediate"

loadYRegZeroPage :: TestTree
loadYRegZeroPage =
    loadYReg [0xA4, 0x0] 0x0 zeroFlag mkTestCPU "Load Y Register - ZeroPage"

loadYRegZeroPageX :: TestTree
loadYRegZeroPageX = loadYReg [0xB4, 0x7F]
                             0x8F
                             negFlag
                             cpu
                             "Load Y Register - ZeroPage - X"
    where cpu = mkTestCPU { xReg = Reg 0x10 }

loadYRegAbsolute :: TestTree
loadYRegAbsolute = loadYReg [0xAC, 0x00, 0xF0]
                            0xAC
                            negFlag
                            mkTestCPU
                            "Load Y Register - Absolute"

loadYRegAbsoluteX :: TestTree
loadYRegAbsoluteX = loadYReg [0xBC, 0x00, 0x30]
                             0x21
                             0x0
                             cpu'
                             "Load Y Register - Absolute - X"
  where
    cpu  = mkTestCPU { xReg = Reg 0xAA }
    mem  = memory cpu // [(0x30AA, 0x21)]
    cpu' = cpu { memory = mem }
{-----------------------------------------------------------------------------------------}


{-------------------------------------- Store Tests --------------------------------------}
