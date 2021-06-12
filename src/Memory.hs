-- | Memory Operations

module Memory
    ( setMemory
    , applyMemory
    , zeroPageX
    , zeroPageY
    , relative
    , absoluteX
    , absoluteY
    , indirectX
    , indirectY
    , indirect
    , implicit
    ) where

import qualified Data.Array.IArray             as IA
import           Lens.Micro.Mtl                 ( (%=) )
import           ProgramCounter
import           Register
import           Types
import           Utils

-- set memory starting at address `addr` with list of Bytes
setMemory :: Address -> [Byte] -> Emulator ()
setMemory addr bytes = do
    let replace =
            [ (addr + fromIntegral i, bytes !! i)
            | i <- [0 .. (length bytes - 1)]
            ]
    #memory %= \m -> m IA.// replace


-- apply a function to a memory location and store the result in the same spot
applyMemory :: Address -> (Byte -> Byte -> Byte) -> Byte -> Emulator Byte
applyMemory addr f byte = do
    val <- fetchByte addr
    let result = f val byte
    setMemory addr [result]
    return result


zeroPageReg :: Byte -> Byte -> Emulator OperandType
zeroPageReg base reg = return $ OpTMemory $ fromIntegral $ base + reg

zeroPageX :: Byte -> Emulator OperandType
zeroPageX base = getXRegister >>= zeroPageReg base

zeroPageY :: Byte -> Emulator OperandType
zeroPageY base = getYRegister >>= zeroPageReg base

relative :: Byte -> Emulator OperandType
relative offset =
    getProgramCounter >>= \pc -> return $ OpTMemory $ fromIntegral offset + pc

absoluteReg :: Byte -> Byte -> Byte -> Emulator OperandType
absoluteReg lsb msb reg =
    return $ OpTMemory $ toAddress lsb msb + fromIntegral reg

absoluteX :: Byte -> Byte -> Emulator OperandType
absoluteX lsb msb = getXRegister >>= absoluteReg lsb msb

absoluteY :: Byte -> Byte -> Emulator OperandType
absoluteY lsb msb = getYRegister >>= absoluteReg lsb msb

indirectX :: Byte -> Emulator OperandType
indirectX base =
    getXRegister >>= \x -> OpTMemory <$> fetchAddress (fromIntegral $ base + x)

indirectY :: Byte -> Emulator OperandType
indirectY base = getYRegister >>= \y -> fetchAddress (fromIntegral base)
    >>= \addr -> return $ OpTMemory $ fromIntegral y + addr

indirect :: Byte -> Byte -> Emulator OperandType
indirect lsb msb = OpTMemory <$> fetchAddress (toAddress lsb msb)

implicit :: Emulator OperandType
implicit = return OpTEmpty
