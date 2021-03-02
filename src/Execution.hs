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
import           Data.Bits                      ( shiftL )
import           Data.Word                      ( )
import           Emulator                       ( Address
                                                , Byte
                                                , CPU(..)
                                                , CPUState(..)
                                                , Emulator
                                                , Flags(..)
                                                , OperandType(..)
                                                , ProgramCounter(..)
                                                , Register(..)
                                                , StackPointer(SP)
                                                , mkCPU
                                                , setMemory
                                                )
import           Flags                          ( FlagType(..)
                                                , isNeg
                                                , updateFlagM
                                                )
import           Instruction                    ( AddressType(..)
                                                , Instruction(..)
                                                , OpCode(..)
                                                , OpName(..)
                                                , Operands(..)
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
runEmulator = runRWST execute []

execute :: Emulator ()
execute = decodeInstruction >>= void . executeInstruction

executeInstruction :: Instruction -> Emulator CPUState
executeInstruction inst@(Instruction (OpCode _ opName _) _) = do
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
        TAX -> do
            (Reg val) <- use #aReg
            updateFlagM (val == 0)  ZF
            updateFlagM (isNeg val) NF
            #xReg .= Reg val
            return Running
        TAY -> do
            (Reg val) <- use #aReg
            updateFlagM (val == 0)  ZF
            updateFlagM (isNeg val) NF
            #yReg .= Reg val
            return Running
        TXA -> do
            (Reg val) <- use #xReg
            updateFlagM (val == 0)  ZF
            updateFlagM (isNeg val) NF
            #aReg .= Reg val
            return Running
        TYA -> do
            (Reg val) <- use #yReg
            updateFlagM (val == 0)  ZF
            updateFlagM (isNeg val) NF
            #aReg .= Reg val
            return Running
        -- Stack Operations
        TSX -> do
            (SP val) <- use #sp
            updateFlagM (val == 0)                 ZF
            updateFlagM (isNeg $ fromIntegral val) NF
            #xReg .= Reg (fromIntegral val)  -- NOTE: fromIntegral Word8 -> Word16 clears high bits (which is what we want)
            return Running
        TXS -> do
            (Reg val) <- use #xReg
            #sp .= SP (0x0100 + fromIntegral val)
            return Running
        PHA -> do
            (Reg val) <- use #aReg
            pushStack val
            sp' <- use #sp
            if spInRange sp'
                then return Running
                else return (CPUError "Stack Overflow")
        PHP -> do
            (Flags val) <- use #fReg
            pushStack val
            sp' <- use #sp
            if spInRange sp'
                then return Running
                else return (CPUError "Stack Overflow")
        PLA -> do
            val <- popStack
            #aReg .= Reg val
            updateFlagM (val == 0)                 ZF
            updateFlagM (isNeg $ fromIntegral val) NF
            sp' <- use #sp
            if spInRange sp'
                then return Running
                else return (CPUError "Stack Underflow")
        PLP -> do
            val <- popStack
            #fReg .= Flags val
            sp' <- use #sp
            if spInRange sp'
                then return Running
                else return (CPUError "Stack Underflow")
        -- Logical Ops
        AND -> do
            undefined
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
    updateFlagM (loadVal == 0)  ZF
    updateFlagM (isNeg loadVal) NF
    return Running

-- | Execute one of: STA | STX | STY
executeStore :: Instruction -> Emulator CPUState
executeStore (Instruction (OpCode _ opName _) (Operands _ (OpTMemory addr))) =
    do
        let register = case opName of
                STA -> #aReg
                STX -> #xReg
                STY -> #yReg
                x   -> error $ show x

        (Reg regVal) <- use register
        setMemory addr [regVal]
        return Running
executeStore i = error $ "executeStore received invalid instruction" <> show i

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
decodeOperand :: OpName -> AddressType -> [Byte] -> Emulator Operands
decodeOperand _ ZeroPage  b@[b1] = return (Operands b (OpTValue b1))
decodeOperand _ ZeroPageX b@[b1] = gets (getReg . xReg)
    >>= \x -> return (Operands b (OpTMemory (fromIntegral $ b1 + x)))
decodeOperand _ ZeroPageY b@[b1] = gets (getReg . yReg)
    >>= \y -> return (Operands b (OpTMemory (fromIntegral $ b1 + y)))
decodeOperand _ Relative b@[b1] = gets (getPC . pc)
    >>= \p -> return (Operands b (OpTMemory (fromIntegral b1 + p)))
decodeOperand _ Absolute b@[b1, b2] =
    return (Operands b (OpTMemory (toAddress b1 b2)))
decodeOperand _ AbsoluteX b@[b1, b2] = gets (getReg . xReg) >>= \x ->
    return (Operands b (OpTMemory (toAddress b1 b2 + fromIntegral x)))
decodeOperand _ AbsoluteY b@[b1, b2] = gets (getReg . yReg) >>= \y ->
    return (Operands b (OpTMemory (toAddress b1 b2 + fromIntegral y)))
decodeOperand _ IndirectX b@[b1] = do
    x    <- fromIntegral . getReg <$> gets xReg
    addr <- fetchAddress $ fromIntegral (b1 + x)
    return (Operands b (OpTMemory addr))
decodeOperand _ IndirectY b@[b1] = do
    y    <- fromIntegral . getReg <$> gets yReg
    addr <- fetchAddress $ fromIntegral b1
    return (Operands b (OpTMemory (addr + y)))
decodeOperand _ Indirect b@[b1, b2] = do
    addr <- fetchAddress $ toAddress b1 b2
    return (Operands b (OpTMemory addr))
decodeOperand _ Immediate b@[b1] = return (Operands b (OpTValue b1))
decodeOperand opn (Implicit _) b = return (Operands b (implicitOperand opn))
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

resolveOperands :: Operands -> Emulator Byte
resolveOperands (Operands _ (OpTValue  val )) = return val
resolveOperands (Operands _ (OpTMemory addr)) = gets (\c -> memory c IA.! addr)
resolveOperands (Operands _ OpTXReg         ) = gets (getReg . xReg)
resolveOperands (Operands _ OpTYReg         ) = gets (getReg . yReg)
resolveOperands (Operands _ OpTAReg         ) = gets (getReg . aReg)
resolveOperands _                             = undefined

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

-- pop value from stack and increment pointer
popStack :: Emulator Byte
popStack = do
    (SP sp') <- gets sp
    mem      <- gets memory
    incrementSP
    return $ mem IA.! sp'

-- push a value on to stack and decrement pointer
pushStack :: Byte -> Emulator ()
pushStack byte = do
    (SP sp') <- gets sp
    #memory %= (\m -> m IA.// [(sp', byte)])
    decrementSP

fetchAddress :: Address -> Emulator Address
fetchAddress addr = do
    mem <- gets memory
    return $ toAddress (mem IA.! addr) (mem IA.! (addr + 1))

spInRange :: StackPointer -> Bool
spInRange sp = spAboveFloor sp && spBelowCeiling sp

spAboveFloor :: StackPointer -> Bool
spAboveFloor (SP sp) = sp >= 0x0100

spBelowCeiling :: StackPointer -> Bool
spBelowCeiling (SP sp) = sp <= 0x01FF
