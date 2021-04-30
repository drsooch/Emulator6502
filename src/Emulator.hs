module Emulator
    ( Emulator
    , Byte
    , Address
    , Offset
    , Memory
    , ProgramCounter(..)
    , StackPointer(..)
    , Register(..)
    , Flags(..)
    , Operand(..)
    , CPU(..)
    , CPUState(..)
    , CPUErrorType(..)
    ,
    -- Constructors
      mkCPU
    ,
    -- functions
      setMemory
    , setProgramCounter
    , setStackPointer
    , setXRegister
    , setYRegister
    , setARegister
    , setFRegister
    ) where

import           Control.Monad.RWS.Strict       ( RWST )
import qualified Data.Array.IArray             as IA
import           Data.Array.Unboxed             ( UArray
                                                , listArray
                                                )
import           Data.Bits                      ( Bits )
import           Data.Generics.Labels           ( )
import           Data.Word                      ( Word16
                                                , Word8
                                                )
import           GHC.Generics                   ( Generic )
import           Lens.Micro.Mtl

-- | Notes on 6502 emulator:
-- | Memory is 64Kb
--     Stack takes up addresses 0x0100 to 0x01FF
-- | Stack Pointer is bottom 8 bits of next free location on stack
--     Note: We use a 16 bit address for the stack pointer
-- | A Register is the accumulator for arithmetic and logical ops (except increment/decrement).
--     Can be stored and retrieved on stack
-- | X Register generally holds counters or offsets for memory access.
--     Can be stored and retrieved on stack, can be compared to values held in memory
--     or increment/decrement ops.
--     Special Function: can get a copy of stack pointer or change its value
-- | Y Register same as X register but no Special Function.
-- | Flags:
--     - Carry Flag: 0x1 -> is set when bit 7 overflows or bit 0 underflows (arithmetic, logical, or comparison)
--     - Zero Flag: 0x2 -> set if last operation was 0
--     - Interrupt Disable Flag: 0x8 -> when bit is set, interrupts are ignored
--     - Decimal Mode Flag: 0x10 -> when bit is set, processor will use BCD in addition/subtraction
--     - Break Flag: 0x20 -> BRK instruction is run and interrupt generated
--     - Overflow Flag: 0x40 -> set if result is invalid 2's complement, looks at carry bit between bit 6 and 7
--                              and between bit 7 and carry flag
--     - Negative Flag: 0x80 -> set if last operation had bit 7 set to 1

-- A fully RESET CPU
mkCPU :: CPU
mkCPU = CPU { .. }
  where
    memory = listArray (0, 0xFFFF) (repeat 0) -- Array of 64kb init to 0
    pc     = PC 0xFFFC
    sp     = SP 0x01FF
    xReg   = Reg 0x0
    yReg   = Reg 0x0
    aReg   = Reg 0x0
    fReg   = Flags 0x0

type Byte = Word8

type Address = Word16

type Offset = Word16

-- main monad for execution
-- Reader/Writer are unused as of now
type Emulator = RWST [String] [String] CPU IO

-- Memory has 16 bit Addresses and 8 bit elements
-- Memory is 64Kb ~ 1024 * 64
type Memory = UArray Address Byte

newtype ProgramCounter = PC {getPC :: Address}
  deriving (Eq, Generic, Show)
  deriving (Num) via Word16

newtype StackPointer = SP {getSP :: Address}
  deriving stock (Eq, Generic, Show)
  deriving (Num) via Word16

newtype Register = Reg {getReg :: Byte}
  deriving stock (Eq, Generic, Show)
  deriving (Num, Bits) via Word8

newtype Flags = Flags {getFlags :: Byte}
  deriving stock (Eq, Generic, Show)
  deriving (Num, Bits) via Word8

-- Where the operand of instruction comes from
data Operand
  = OpTXReg
  | OpTYReg
  | OpTAReg
  | OpTFReg
  | OpTStack
  | NoOperand
  | OpTMemory Address
  | OpTValue Byte
  deriving (Eq, Show)

-- State of the Underlying CPU
data CPUState
  = Running
  | Stopped
  | CPUError CPUErrorType
  deriving (Eq, Show)

data CPUErrorType = StackOverflow
                  | StackUnderflow
                  deriving(Eq, Show)

data CPU = CPU
    { -- Memory of 6502 - 64Kb
      memory :: Memory
    ,
    -- Program Counter (16 bits)
      pc     :: ProgramCounter
    ,
    -- Stack Pointer (8 bits)
      sp     :: StackPointer
    ,
    -- X Register
      xReg   :: Register
    ,
    -- Y Register
      yReg   :: Register
    ,
    -- Accumulator Register
      aReg   :: Register
    ,
    -- 7 bits denote flags
      fReg   :: Flags
    }
    deriving (Eq, Generic)

instance Show CPU where
    show cpu =
        "CPU{ "
            <> "Program Counter="
            <> show (pc cpu)
            <> "Stack Pointer="
            <> show (sp cpu)
            <> "A Register="
            <> show (aReg cpu)
            <> "X Register="
            <> show (xReg cpu)
            <> "Y Register="
            <> show (yReg cpu)
            <> "}"

-- set a range of addresses with byte values
setMemory :: Address -> [Byte] -> Emulator ()
setMemory addr bytes = do
    let replace =
            [ (addr + fromIntegral i, bytes !! i)
            | i <- [0 .. (length bytes - 1)]
            ]
    #memory %= (\m -> m IA.// replace)

setStackPointer :: Address -> Emulator ()
setStackPointer addr = #sp .= SP addr

setProgramCounter :: Address -> Emulator ()
setProgramCounter addr = #pc .= PC addr

setARegister :: Byte -> Emulator ()
setARegister byte = #aReg .= Reg byte

setXRegister :: Byte -> Emulator ()
setXRegister byte = #xReg .= Reg byte

setYRegister :: Byte -> Emulator ()
setYRegister byte = #yReg .= Reg byte

setFRegister :: Byte -> Emulator ()
setFRegister byte = #fReg .= Flags byte
