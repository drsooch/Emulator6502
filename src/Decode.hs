-- | Decode Instructions and Operations

module Decode
    ( decodeInstruction
    , resolveOperands
    , boolToByte
    ) where

import           Control.Monad.RWS
import qualified Data.Array.IArray             as IA
import           Data.Bits                      ( shiftL )
import           Instruction
import           Lens.Micro.Mtl
import           ProgramCounter
import           Types


-- | Decode an address
-- | ZeroPage -> we want to keep them as bytes so we wrap on the
-- |          ZeroPage boundary
-- | Relative -> applies an offset to current PC
-- | Absolute -> converts 2 bytes into an address
-- | AbsoluteX/Y -> converts 2 bytes into an address and applies offset
-- |             from X or Y register
-- | Indirect -> constructs address from 2 bytes, the result is then
-- |            an address of the MSB of the indirect address.
-- |            Then move the address one byte to get the LSB
-- |            of indirect address, then combine
-- | IndirectX/Y -> constructs an indirect address that is in the ZeroPage
-- | Immediate   -> returns the exact byte
decodeOperand :: OpName -> AddressType -> [Byte] -> Emulator OperandType
decodeOperand _ ZeroPage [b1] = return (OpTMemory $ fromIntegral b1)
decodeOperand _ ZeroPageX [b1] =
    use #xReg >>= (\x -> return (OpTMemory (fromIntegral $ b1 + x))) . getReg
decodeOperand _ ZeroPageY [b1] =
    use #yReg >>= (\y -> return (OpTMemory (fromIntegral $ b1 + y))) . getReg
decodeOperand _ Relative [b1] =
    use #pc >>= (\p -> return (OpTMemory (fromIntegral b1 + p))) . getPC
decodeOperand _ Absolute [b1, b2] = return (OpTMemory (toAddress b1 b2))
decodeOperand _ AbsoluteX [b1, b2] =
    use #xReg
        >>= (\x -> return (OpTMemory (toAddress b1 b2 + fromIntegral x)))
        .   getReg
decodeOperand _ AbsoluteY [b1, b2] =
    use #yReg
        >>= (\y -> return (OpTMemory (toAddress b1 b2 + fromIntegral y)))
        .   getReg
-- FIXME: Indirect gets the low byte of an address need upper bits
decodeOperand _ IndirectX [b1] = do
    x    <- getReg <$> use #xReg
    addr <- fetchAddress $ fromIntegral (b1 + x)
    return (OpTMemory addr)
decodeOperand _ IndirectY [b1] = do
    y    <- fromIntegral . getReg <$> use #yReg
    addr <- fetchAddress $ fromIntegral b1
    return (OpTMemory (addr + y))
decodeOperand _ Indirect [b1, b2] = do
    addr <- fetchAddress $ toAddress b1 b2
    return (OpTMemory addr)
decodeOperand _   Immediate [b1] = return (OpTValue b1)
decodeOperand opn Implicit  _    = return OpTEmpty
decodeOperand opn addr b =
    error
        $  "Invalid Operand with: "
        <> show opn
        <> " "
        <> show addr
        <> " "
        <> show b

-- decode an Instruction and grab the inputs based on AddressType
decodeInstruction :: Emulator Instruction
decodeInstruction = do
    CPU {..}               <- get
    op@(OpCode _ opn addr) <- decodeOpCode <$> fetchByte (getPC pc)
    ops                    <- sequenceA $ case operandNum addr of
        2 -> [fetchByte (getPC pc + 1), fetchByte (getPC pc + 2)]
        1 -> [fetchByte (getPC pc + 1)]
        _ -> []
    incrementPC (length ops)
    Instruction op <$> decodeOperand opn addr ops

-- unwrap an OperandType
resolveOperands :: OperandType -> Emulator Byte
resolveOperands (OpTValue  val ) = return val
resolveOperands (OpTMemory addr) = gets (\c -> memory c IA.! addr)
resolveOperands OpTEmpty         = return 0 -- if we ever call this we've done something wrong

fetchByte :: Address -> Emulator Byte
fetchByte addr = use #memory >>= \mem -> return $ mem IA.! addr

fetchAddress :: Address -> Emulator Address
fetchAddress addr = do
    mem <- use #memory
    return $ toAddress (mem IA.! addr) (mem IA.! (addr + 1))

-- convert two bytes into a 2 byte address
-- Little Endian
toAddress :: Byte -> Byte -> Address
toAddress lsb msb = fromIntegral msb `shiftL` 8 + fromIntegral lsb

boolToByte :: Bool -> Byte
boolToByte True  = 1
boolToByte False = 0
