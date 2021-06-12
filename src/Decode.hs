-- | Decode Instructions and Operations

module Decode
    ( decodeInstruction
    , resolveOperand
    , storeValue
    , applyOperation
    ) where

import           Control.Monad.RWS              ( MonadState(get)
                                                , void
                                                )
import           Flags
import           Instruction
import           Memory
import           ProgramCounter
import           Register
import           Stack
import           Types
import           Utils


-- | Decode the operand into it's byte value and capture any information
-- | regarding where we store the information.
-- |
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
decodeOperand :: OpCode -> AddressType -> [Byte] -> Emulator Operand
decodeOperand opc addrT bs = case (addrT, bs) of
    (ZeroPage, [b1]) -> return $ Operand
        (OpTMemory (fromIntegral b1))
        (decodeStoreLoc opc (OpTMemory $ fromIntegral b1) ZeroPage)
    (ZeroPageX, [b1]) -> zeroPageX b1
        >>= \addr -> return $ Operand addr (decodeStoreLoc opc addr addrT)
    (ZeroPageY, [b1]) ->
        zeroPageY b1 >>= \addr ->
            return $ Operand addr (decodeStoreLoc opc addr addrT)
    (Relative, [b1]) ->
        relative b1 >>= \addr ->
            return $ Operand addr (decodeStoreLoc opc addr addrT)
    (Absolute, [b1, b2]) -> return $ Operand
        (OpTMemory $ toAddress b1 b2)
        (decodeStoreLoc opc (OpTMemory $ toAddress b1 b2) addrT)
    (AbsoluteX, [b1, b2]) -> absoluteX b1 b2
        >>= \addr -> return $ Operand addr (decodeStoreLoc opc addr addrT)
    (AbsoluteY, [b1, b2]) -> absoluteY b1 b2
        >>= \addr -> return $ Operand addr (decodeStoreLoc opc addr addrT)
    (IndirectX, [b1]) ->
        indirectX b1 >>= \addr ->
            return $ Operand addr (decodeStoreLoc opc addr addrT)
    (IndirectY, [b1]) ->
        indirectY b1 >>= \addr ->
            return $ Operand addr (decodeStoreLoc opc addr addrT)
    (Indirect, [b1, b2]) -> indirect b1 b2
        >>= \addr -> return $ Operand addr (decodeStoreLoc opc addr addrT)
    (Immediate, [b1]) -> return
        $ Operand (OpTValue b1) (decodeStoreLoc opc (OpTValue b1) addrT)
    (Implicit, _) ->
        return $ Operand OpTEmpty (decodeStoreLoc opc OpTEmpty addrT)
    (Accumulator, _) ->
        return $ Operand OpTAccumulator (decodeStoreLoc opc OpTEmpty addrT)
    (_, _) -> error "Invalid Operands"

resolveOperand :: OperandType -> Emulator Byte
resolveOperand op = case op of
    (OpTValue  b   ) -> return b
    (OpTMemory addr) -> fetchByte addr
    OpTAccumulator   -> getARegister
    OpTEmpty         -> return $ error "OpTEmpty is used..."

-- decode an Instruction and grab the inputs based on AddressType
decodeInstruction :: Emulator Instruction
decodeInstruction = do
    CPU {..}           <- get
    op@(OpCode _ addr) <- decodeOpCode <$> (getProgramCounter >>= fetchByte)
    ops                <- sequenceA $ case operandNum addr of
        2 -> [fetchByte (getPC pc + 1), fetchByte (getPC pc + 2)]
        1 -> [fetchByte (getPC pc + 1)]
        _ -> []
    incrementPC (length ops)
    Instruction op <$> decodeOperand op addr ops


applyOperation :: StoreLoc -> (Byte -> Byte -> Byte) -> Byte -> Emulator Byte
applyOperation storeLoc f val = case storeLoc of
    (MemorySL   addr)    -> applyMemory addr f val
    (RegisterSL AReg)    -> applyARegister f val
    (RegisterSL XReg)    -> applyXRegister f val
    (RegisterSL YReg)    -> applyYRegister f val
    (RegisterSL FReg)    -> setFRegister val >> return val
    -- FIXME
    StackPointerSL       -> undefined
    (ProgramCounterSL _) -> undefined
    NoStore              -> undefined

storeValue :: StoreLoc -> Byte -> Emulator ()
storeValue storeLoc val = case storeLoc of
    (MemorySL   addr)       -> setMemory addr [val]
    (RegisterSL AReg)       -> void $ setARegister val
    (RegisterSL XReg)       -> void $ setXRegister val
    (RegisterSL YReg)       -> void $ setYRegister val
    (RegisterSL FReg)       -> void $ setFRegister val
    -- FIXME
    StackPointerSL          -> setStackPointer val
    (ProgramCounterSL addr) -> setProgramCounter addr
    NoStore                 -> return ()
