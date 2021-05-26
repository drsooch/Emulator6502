-- | Utility functions that span all tests
module TestUtils
    ( mkTestCPU
    , setTestZeroPage
    , setProgramMemory
    ) where


import           Data.Array.IArray              ( (//)
                                                , listArray
                                                )
import           Types                          ( Byte
                                                , CPU(..)
                                                , Flags(Flags)
                                                , Memory
                                                , ProgramCounter(PC)
                                                , Register(Reg)
                                                , StackPointer(SP)
                                                , getPC
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
    let zp = [ (ix, 0xFF - fromIntegral ix) | ix <- [0 .. 0xFF] ] in mem // zp

setProgramMemory :: [Byte] -> CPU -> CPU
setProgramMemory bytes c@CPU {..} =
    let newValues = zip [(getPC pc) ..] bytes
        memory'   = memory // newValues
    in  c { memory = memory' }
