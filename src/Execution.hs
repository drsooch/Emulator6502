module Execution
    ( runEmulator
    , hardResetCPU
    , execute
    ) where

import           Control.Monad.RWS.Strict       ( MonadState(get)
                                                , RWST(runRWST)
                                                , void
                                                )
import           Control.Monad.State.Strict     ( gets )
import qualified Data.Array.IArray             as IA
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , shiftL
                                                , xor
                                                )
import           Data.Word                      ( )
import           Emulator                       ( Address
                                                , Byte
                                                , CPU(..)
                                                , CPUErrorType(..)
                                                , CPUState(..)
                                                , Emulator
                                                , Flags(..)
                                                , Operand(..)
                                                , ProgramCounter(..)
                                                , Register(..)
                                                , StackPointer(SP, getSP)
                                                , mkCPU
                                                , setMemory
                                                )
import           Flags                          ( FlagType(..)
                                                , decFlag
                                                , isNegative
                                                , isOverflow
                                                , updateFlag
                                                )
import           Instruction                    ( AddressType(..)
                                                , Instruction(..)
                                                , OpCode(..)
                                                , OpName(..)
                                                , decodeOpCode
                                                , implicitOperand
                                                , operandNum
                                                )
import           Lens.Micro.Mtl                 ( (%=)
                                                , (+=)
                                                , (-=)
                                                , (.=)
                                                , use
                                                )

-- full "factory" reset
hardResetCPU :: CPU -> CPU
hardResetCPU _ = cpu { memory = mem' }
  where
    cpu  = mkCPU
    mem  = memory cpu
    mem' = mem IA.// [(0xFFFC, 0xA9), (0xFFFD, 57)]

-- -- resets everything but memory
-- softResetCPU :: CPU -> CPU
-- softResetCPU CPU {..} = let c = mkCPU in c { memory = memory }

runEmulator :: CPU -> IO ((), CPU, [String])
runEmulator = runRWST (void execute) []

execute :: Emulator CPUState
execute = decodeInstruction >>= executeInstruction

executeInstruction :: Instruction -> Emulator CPUState
executeInstruction inst@(Instruction (OpCode _ opName _) opt) = do
    CPU {..} <- get
    case opName of
        -- Load Ops
        LDA -> executeLoad inst
        LDX -> executeLoad inst
        LDY -> executeLoad inst
        -- Store Ops
        STA -> executeStore inst
        STX -> executeStore inst
        STY -> executeStore inst
        -- Transfer Ops
        TAX -> executeTransfer inst
        TAY -> executeTransfer inst
        TXA -> executeTransfer inst
        TYA -> executeTransfer inst
        -- Stack Operations
        TSX -> do
            val <- fromIntegral . getSP <$> use #sp
            updateFlag (val == 0)       ZF
            updateFlag (isNegative val) NF
            #xReg .= Reg (fromIntegral val)  -- NOTE: fromIntegral Word8 -> Word16 clears high bits (which is what we want)
            return Running
        TXS -> do
            (Reg val) <- use #xReg
            #sp .= SP (0x0100 + fromIntegral val)
            return Running
        PHA -> use #aReg >>= pushStack . getReg >> spInRangeM
        PHP -> use #fReg >>= pushStack . getFlags >> spInRangeM
        PLA -> do
            val <- popStack
            #aReg .= Reg val
            updateFlag (val == 0)       ZF
            updateFlag (isNegative val) NF
            spInRangeM
        PLP -> do
            val <- popStack
            #fReg .= Flags val
            spInRangeM
        -- Logical Ops
        AND -> executeLogicalOp inst (.&.)
        EOR -> executeLogicalOp inst xor
        ORA -> executeLogicalOp inst (.|.)
        BIT -> do
            aVal  <- getReg <$> use #aReg
            opVal <- resolveOperands opt
            let result = aVal .&. opVal
            updateFlag (result == 0)       ZF
            updateFlag (isNegative result) NF
            updateFlag (isOverflow opVal)  OF  -- this is the same as testBit but I don't know if Data.Bits uses 0 index
            return Running
        ADC ->
            use #fReg >>= \f ->
                if (f .&. decFlag) == 1 then addDecimal else addBinary

        _ -> error $ "Instruction hasn't been implemented" <> show inst

-- | Execute one of: LDX | LDA | LDY
executeLoad :: Instruction -> Emulator CPUState
executeLoad (Instruction (OpCode _ opName _) ops) = do
    loadVal <- resolveOperands ops
    let register = case opName of
            LDX -> #xReg
            LDY -> #yReg
            LDA -> #aReg
            x   -> error $ show x
    register .= Reg loadVal
    updateFlag (loadVal == 0)       ZF
    updateFlag (isNegative loadVal) NF
    return Running

-- | Execute one of: STA | STX | STY
executeStore :: Instruction -> Emulator CPUState
executeStore (Instruction (OpCode _ opName _) (OpTMemory addr)) = do
    let register = case opName of
            STA -> #aReg
            STX -> #xReg
            STY -> #yReg
            x   -> error $ show x
    (Reg regVal) <- use register
    setMemory addr [regVal]
    return Running
-- can't use resolveOperands as Store requires a memory address
executeStore i =
    error $ "executeStore received invalid instruction: " <> show i

-- | Execute one of: TAX | TAY | TXA | TYA
executeTransfer :: Instruction -> Emulator CPUState
executeTransfer (Instruction (OpCode _ opName _) _) = do
    let (fromReg, toReg) = case opName of
            TAX -> (#aReg, #xReg)
            TAY -> (#aReg, #yReg)
            TXA -> (#xReg, #aReg)
            TYA -> (#yReg, #aReg)
            _   -> undefined
    (Reg val) <- use fromReg
    updateFlag (val == 0)       ZF
    updateFlag (isNegative val) NF
    toReg .= Reg val
    return Running

-- | Execute one of: AND | EOR | ORA
executeLogicalOp :: Instruction -> (Byte -> Byte -> Byte) -> Emulator CPUState
executeLogicalOp (Instruction _ opt) logOp = do
    aVal  <- getReg <$> use #aReg
    opVal <- resolveOperands opt
    let result = logOp aVal opVal
    updateFlag (result == 0x0)     ZF
    updateFlag (isNegative result) NF
    #aReg .= Reg result
    return Running

addBinary :: Emulator CPUState
addBinary = undefined

addDecimal :: Emulator CPUState
addDecimal = undefined

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
decodeOperand :: OpName -> AddressType -> [Byte] -> Emulator Operand
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
decodeOperand opn Implicit  _    = return (implicitOperand opn)
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

-- Get the Byte value associated with Operand
-- Most instructions will have to inspect directly the Operand
-- but some instructions require a byte
resolveOperands :: Operand -> Emulator Byte
resolveOperands (OpTValue  val ) = return val
resolveOperands (OpTMemory addr) = gets (\c -> memory c IA.! addr)
resolveOperands OpTXReg          = gets (getReg . xReg)
resolveOperands OpTYReg          = gets (getReg . yReg)
resolveOperands OpTAReg          = gets (getReg . aReg)
resolveOperands _                = undefined

incrementPC :: Int -> Emulator ()
incrementPC num = #pc %= (+) (1 + fromIntegral num)

incrementSP :: Emulator ()
incrementSP = #sp += 1

decrementSP :: Emulator ()
decrementSP = #sp -= 1

-- convert two bytes into a 2 byte address
-- Little Endian
toAddress :: Byte -> Byte -> Address
toAddress lsb msb = fromIntegral msb `shiftL` 8 + fromIntegral lsb

fetchByte :: Address -> Emulator Byte
fetchByte addr = do
    mem <- gets memory
    return $ mem IA.! addr

fetchAddress :: Address -> Emulator Address
fetchAddress addr = do
    mem <- use #memory
    return $ toAddress (mem IA.! addr) (mem IA.! (addr + 1))

-- pop value from stack and increment pointer
popStack :: Emulator Byte
popStack = do
    (SP sp') <- use #sp
    mem      <- use #memory
    incrementSP
    return $ mem IA.! sp'

-- push a value on to stack and decrement pointer
pushStack :: Byte -> Emulator ()
pushStack byte = do
    (SP sp') <- use #sp
    #memory %= (\m -> m IA.// [(sp', byte)])
    decrementSP

spInRangeM :: Emulator CPUState
spInRangeM = do
    sp' <- use #sp
    return $ if
        | spBelowFloor sp'   -> CPUError StackOverflow
        | spAboveCeiling sp' -> CPUError StackUnderflow
        | otherwise          -> Running


-- spInRange :: StackPointer -> Bool
-- spInRange sp = spAboveFloor sp && spBelowCeiling sp

spBelowFloor :: StackPointer -> Bool
spBelowFloor (SP sp) = sp < 0x0100

spAboveCeiling :: StackPointer -> Bool
spAboveCeiling (SP sp) = sp > 0x01FF
