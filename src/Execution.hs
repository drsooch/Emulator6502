module Execution
    ( runEmulator
    , hardResetCPU
    , execute
    ) where

import           Control.Monad.RWS.Strict       ( RWST(runRWST)
                                                , void
                                                )
import qualified Data.Array.IArray             as IA
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , shiftL
                                                , shiftR
                                                , testBit
                                                , xor
                                                )
import           Data.Word                      ( )

import           Decode
import           Flags
import           Instruction
import           Lens.Micro.Mtl                 ( (%=)
                                                , (.=)
                                                , (<%=)
                                                , (<.=)
                                                , assign
                                                , use
                                                )
import           Stack
import           Types

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
            #fReg %= updateFlag (isZero val) ZF
            #fReg %= updateFlag (isNegative val) NF
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
            #fReg %= updateFlag (isZero val) ZF
            #fReg %= updateFlag (isNegative val) NF
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
            -- BIT is weird... Updates ZERO FLAG if A Reg & OpValue is Zero
            -- Then set Negative Flag to bit 7 of OpValue
            -- Then set Overflow Flag to bit 6 of OpValue
            aVal  <- getReg <$> use #aReg
            opVal <- resolveOperands opt
            let result = aVal .&. opVal
            #fReg %= updateFlag (isZero result) ZF
            #fReg %= updateFlag (isNegative opVal) NF
            #fReg %= updateFlag (testBit opVal 6) OF  -- Not a fan of this
            return Running
        -- Arithmetic Ops
        ADC -> isFlagSet DF
            >>= \dec -> if dec then addDecimal inst else addBinary inst
        SBC -> isFlagSet DF
            >>= \dec -> if dec then subDecimal inst else subBinary inst
        -- not sure why this is defined as Arithmetic Op but oh well.
        CMP -> executeCompare inst
        CPX -> executeCompare inst
        CPY -> executeCompare inst
        -- Increment/Decrement Ops
        INC -> case opt of
            OpTMemory addr -> executeIncOrDecMem (+) opt addr
            _ -> error $ "Invalid OperandType for INC: " <> show inst
        INX -> executeIncOrDecReg (+) #xReg
        INY -> executeIncOrDecReg (+) #yReg
        DEC -> case opt of
            OpTMemory addr -> executeIncOrDecMem (-) opt addr
            _ -> error $ "Invalid OperandType for DEC: " <> show inst
        DEX -> executeIncOrDecReg (-) #xReg
        DEY -> executeIncOrDecReg (-) #yReg
        -- Shift Ops
        ASL -> undefined
        LSR -> undefined
        ROL -> undefined
        ROR -> undefined
        _   -> error $ "Instruction hasn't been implemented" <> show inst


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
    #fReg %= updateFlag (isZero loadVal) ZF
    #fReg %= updateFlag (isNegative loadVal) NF
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
    let
        (fromReg, toReg) = case opName of
            TAX -> (#aReg, #xReg)
            TAY -> (#aReg, #yReg)
            TXA -> (#xReg, #aReg)
            TYA -> (#yReg, #aReg)
            invalid ->
                error
                    $  "Invalid instruction in executeTransfer: "
                    <> show invalid
    toVal@(Reg val) <- use fromReg
    #fReg %= updateFlag (isZero val) ZF
    #fReg %= updateFlag (isNegative val) NF
    toReg .= toVal
    return Running

-- | Execute one of: AND | EOR | ORA
executeLogicalOp :: Instruction -> (Byte -> Byte -> Byte) -> Emulator CPUState
executeLogicalOp (Instruction _ opt) logOp = do
    aVal         <- getReg <$> use #aReg
    opVal        <- resolveOperands opt
    (Reg result) <- #aReg <.= (Reg $ logOp aVal opVal)
    #fReg %= updateFlag (isZero result) ZF
    #fReg %= updateFlag (isNegative result) NF
    return Running

-- | Execute ADC with DF clear
addBinary :: Instruction -> Emulator CPUState
addBinary (Instruction _ opt) = do
    aVal         <- getReg <$> use #aReg
    adder        <- resolveOperands opt
    carry        <- boolToByte <$> isFlagSet CF
    (Reg result) <- #aReg <.= (Reg $ aVal + adder + carry)
    #fReg %= updateFlag (isZero result) ZF
    #fReg %= updateFlag (isNegative result) NF
    #fReg %= updateFlag (isOverflow aVal adder result) OF
    #fReg %= updateFlag (isCarryAdd aVal result carry) CF
    return Running

-- | Execute SBC with DF clear
subBinary :: Instruction -> Emulator CPUState
subBinary (Instruction _ opt) = do
    aVal         <- getReg <$> use #aReg
    subber       <- resolveOperands opt
    carry        <- boolToByte . not <$> isFlagSet CF
    (Reg result) <- #aReg <.= (Reg $ aVal - subber - carry)
    #fReg %= updateFlag (isZero result) ZF
    #fReg %= updateFlag (isNegative result) NF
    #fReg %= updateFlag (isOverflow aVal subber result) OF
    #fReg %= updateFlag (isCarrySub aVal subber) CF
    return Running

-- | Execute ADC with DF set
addDecimal :: Instruction -> Emulator CPUState
addDecimal (Instruction _ opt) = do
    aVal  <- getReg <$> use #aReg
    adder <- resolveOperands opt
    carry <- boolToByte <$> isFlagSet CF
    let onesDigit = (aVal .&. 0xF) + (adder .&. 0xF) + carry  -- compute lower bits addition
    let (onesCarry, onesDigit') = if onesDigit .&. 0xF > 0    -- if we carry into upper bits, we add 0x6 to onesDigits to simulate wrapping
            then (0x1, onesDigit + 0x6)
            else (0x0, onesDigit)
    let tensDigit = (aVal `shiftR` 4) + (adder `shiftR` 4) + onesCarry  -- compute upper bits
    let (bcdCarry, tensDigit') = if tensDigit .&. 0xF > 0 -- if we carry into upper bits, we add 0x6 to tensDigit to simulate wrapping
            then (True, tensDigit + 0x6)
            else (False, tensDigit)
    (Reg result) <- #aReg <.= (Reg $ tensDigit' `shiftL` 4 + onesDigit')
    #fReg %= updateFlag (isZero result) ZF
    #fReg %= updateFlag (isNegative result) NF  -- 6502 inspects BCD bit 7 even though it doesn't make much sense
    #fReg %= updateFlag False OF                -- 6502 overflow flag is undefined for BCD
    #fReg %= updateFlag bcdCarry CF
    return Running

-- | Execute SBC with DF set
subDecimal :: Instruction -> Emulator CPUState
subDecimal (Instruction _ opt) = do
    _     <- error "SUB DECIMAL not implemented"
    aVal  <- getReg <$> use #aReg
    adder <- resolveOperands opt
    carry <- boolToByte . not <$> isFlagSet CF
    let onesDigit = (aVal .&. 0xF) - (adder .&. 0xF) - carry  -- compute lower bits addition
    let (onesCarry, onesDigit') = if onesDigit .&. 0xF > 0    -- if we carry into upper bits, we add 0x6 to onesDigits to simulate wrapping
            then (0x1, onesDigit + 0x6)
            else (0x0, onesDigit)
    let tensDigit = (aVal `shiftR` 4) + (adder `shiftR` 4) + onesCarry  -- compute upper bits
    let (bcdCarry, tensDigit') = if tensDigit .&. 0xF > 0 -- if we carry into upper bits, we add 0x6 to tensDigit to simulate wrapping
            then (True, tensDigit + 0x6)
            else (False, tensDigit)
    (Reg result) <- #aReg <.= (Reg $ tensDigit' `shiftL` 4 + onesDigit')
    #fReg %= updateFlag (isZero result) ZF
    #fReg %= updateFlag (isNegative result) NF  -- 6502 inspects BCD bit 7 even though it doesn't make much sense
    #fReg %= updateFlag False OF                -- 6502 overflow flag is undefined for BCD
    #fReg %= updateFlag bcdCarry CF
    return Running

-- | Execute one of: CMP | CPY | CPX
executeCompare :: Instruction -> Emulator CPUState
executeCompare (Instruction opCode opt) =
    executeCompare' opt . getReg =<< case getOpName opCode of
        CMP -> use #aReg
        CPY -> use #yReg
        CPX -> use #xReg
        invalid ->
            error
                $  "Invalid instruction called with executeCompare: "
                <> show invalid

executeCompare' :: OperandType -> Byte -> Emulator CPUState
executeCompare' opt regVal = do
    cmpVal <- resolveOperands opt
    #fReg %= updateFlag (regVal >= cmpVal) CF
    #fReg %= updateFlag (regVal == cmpVal) ZF
    #fReg %= updateFlag (isNegative $ regVal - cmpVal) NF
    return Running

executeIncOrDecReg
    :: (Register -> Register -> Register) -> RegisterName -> Emulator CPUState
executeIncOrDecReg binOp reg = do
    (Reg result) <- reg <%= binOp (Reg 1)
    #fReg %= updateFlag (isZero result) ZF
    #fReg %= updateFlag (isNegative result) NF
    return Running

executeIncOrDecMem
    :: (Byte -> Byte -> Byte) -> OperandType -> Address -> Emulator CPUState
executeIncOrDecMem binOp opt addr = do
    curVal <- resolveOperands opt
    let result = curVal `binOp` 1
    #fReg %= updateFlag (isZero result) ZF
    #fReg %= updateFlag (isNegative result) NF
    setMemory addr [result]
    return Running
