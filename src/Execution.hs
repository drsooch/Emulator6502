module Execution
    ( runEmulator
    , execute
    ) where

import           Control.Monad                  ( unless
                                                , void
                                                , when
                                                )
import           Control.Monad.State.Strict     ( runStateT )
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , testBit
                                                , xor
                                                )
import           Data.Bool                      ( bool )
import           Data.Text                      ( pack )
import           Decode
import           Flags
import           Instruction
import           Logging
import           ProgramCounter
import           Register
import           Stack
import           Types
import           Utils

runEmulator :: CPUState -> IO ((), CPUState)
runEmulator = runStateT (void execute)

execute :: Emulator ()
execute = logCPUState >> decodeInstruction >>= \inst ->
    logInstruction inst >> executeInstruction inst >> logCPUState

executeInstruction :: Instruction -> Emulator ()
executeInstruction inst@(Instruction (OpCode opName _) operand@(Operand opT storeLoc))
    = case opName of
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
            val <- fromIntegral <$> getStackPointer
            updateFlag (isZero val)     ZF

            updateFlag (isNegative val) NF
            void $ setXRegister (fromIntegral val)
        TXS -> do
            val <- getXRegister
            setStackPointer val
        PHA -> getARegister >>= pushStackByte
        PHP -> getFRegister >>= pushStackByte
        PLA -> do
            val <- popStackByte
            void $ setARegister val
            updateFlag (isZero val)     ZF
            updateFlag (isNegative val) NF
        PLP -> do
            val <- popStackByte
            setFRegister val
        -- Logical Ops
        AND -> executeLogicalOp inst (.&.)
        EOR -> executeLogicalOp inst xor
        ORA -> executeLogicalOp inst (.|.)
        BIT -> do
            -- BIT is weird... Updates ZERO FLAG if A Reg & OpValue is Zero
            -- Then set Negative Flag to bit 7 of OpValue
            -- Then set Overflow Flag to bit 6 of OpValue
            aVal  <- getARegister
            opVal <- resolveOperand opT
            updateFlag (isZero (aVal .&. opVal)) ZF
            updateFlag (isNegative opVal)        NF
            updateFlag (testBit opVal 6)         OF  -- Not a fan of this
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
        INC -> executeIncOrDec (+) storeLoc
        INX -> executeIncOrDec (+) storeLoc
        INY -> executeIncOrDec (+) storeLoc
        DEC -> executeIncOrDec (-) storeLoc
        DEX -> executeIncOrDec (-) storeLoc
        DEY -> executeIncOrDec (-) storeLoc
        -- Shift Ops
        ASL -> do
            (unshift, result) <- executeShift shiftL operand
            storeValue storeLoc result
            updateFlag (isNegative unshift) CF -- Carry Flag is set to bit 7 (same as isNegative)
            updateFlag (isZero result)      ZF
            updateFlag (isNegative result)  NF
        LSR -> do
            (unshift, result) <- executeShift shiftR operand
            storeValue storeLoc result
            updateFlag (testBit unshift 0) CF -- CF is equal to bit Zero of unshifted
            updateFlag (isZero result)     ZF
            updateFlag (isNegative result) NF -- this should always be False
        ROL -> do
            unrotated <- resolveOperand opT
            carry     <- bool 0 1 <$> isFlagSet CF
            -- rotFn ignores second value as applyOperation is a binaryOp
            -- could also ignore first value also but whatever
            writeLog $ "Value of Carry: " <> pack (show carry)
            let rotFn = \unrot _ -> (unrot `shiftL` 1) .|. carry
            result <- applyOperation storeLoc rotFn 0
            updateFlag (isNegative unrotated) CF
            updateFlag (isZero result)        ZF
            updateFlag (isNegative result)    NF
        ROR -> do
            unrotated <- resolveOperand opT
            -- if carry is set we will set bit 7
            carry     <- bool 0 0x80 <$> isFlagSet CF
            -- see above
            let rotFn = \unrot _ -> (unrot `shiftR` 1) .|. carry
            result <- applyOperation storeLoc rotFn 0
            updateFlag (testBit unrotated 0) CF
            updateFlag (isZero result)       ZF
            updateFlag (isNegative result)   NF
        -- Jump Ops
        JMP ->
            -- the StoreLoc of Operand holds jump point
            -- requires a dummy byte val
            -- yuck.
            storeValue storeLoc 0
        JSR ->
            -- push current PC onto stack (minus one for some reason)
            -- TODO: figure out why
            getProgramCounter >>= pushStackAddress . flip (-) 1
               -- see JMP not sure if there's even a point to adding and subtracting 1
        RTS -> popStackAddress >>= setProgramCounter . (+) 1
        -- Branch Ops
        BCC -> isFlagSet CF >>= \flag -> unless flag $ storeValue storeLoc 0
        BCS -> isFlagSet CF >>= \flag -> when flag $ storeValue storeLoc 0
        BEQ -> isFlagSet ZF >>= \flag -> when flag $ storeValue storeLoc 0
        BMI -> isFlagSet NF >>= \flag -> when flag $ storeValue storeLoc 0
        BNE -> isFlagSet ZF >>= \flag -> unless flag $ storeValue storeLoc 0
        BPL -> isFlagSet NF >>= \flag -> unless flag $ storeValue storeLoc 0
        BVC -> isFlagSet OF >>= \flag -> unless flag $ storeValue storeLoc 0
        BVS -> isFlagSet OF >>= \flag -> when flag $ storeValue storeLoc 0       -- Status Flag Ops
        CLC -> clearFlag CF
        CLD -> clearFlag DF
        CLI -> clearFlag IF
        CLV -> clearFlag OF
        SEC -> setFlag CF
        SED -> setFlag DF
        SEI -> setFlag IF
        -- System Ops
        -- push Flags and PC to Stack, jump to IRQ handler
        BRK ->
            getProgramCounter
                >>= pushStackAddress
                >>  getFRegister
                >>= pushStackByte
                >>  setProgramCounter 0xFFFE
        NOP -> return ()
        -- pop Flags then PC and return to break point
        RTI ->
            popStackByte
                >>= setFRegister
                >>  popStackAddress
                >>= setProgramCounter


-- | Execute one of: LDX | LDA | LDY
executeLoad :: Instruction -> Emulator ()
executeLoad (Instruction _ (Operand opT storeLoc)) = do
    loadVal <- resolveOperand opT
    storeValue storeLoc loadVal
    updateFlag (isZero loadVal)     ZF
    updateFlag (isNegative loadVal) NF

-- | Execute one of: STA | STX | STY
executeStore :: Instruction -> Emulator ()
executeStore (Instruction (OpCode opName _) (Operand _ storeLoc)) = do
    regVal <- case opName of
        STA -> getARegister
        STX -> getXRegister
        STY -> getYRegister
        x   -> error $ show x
    storeValue storeLoc regVal

-- | Execute one of: TAX | TAY | TXA | TYA
executeTransfer :: Instruction -> Emulator ()
executeTransfer (Instruction (OpCode opName _) (Operand _ storeLoc)) = do
    val <- case opName of
        TAX -> getARegister
        TAY -> getARegister
        TXA -> getXRegister
        TYA -> getYRegister
        invalid ->
            error $ "Invalid instruction in executeTransfer: " <> show invalid
    storeValue storeLoc val
    updateFlag (isZero val)     ZF
    updateFlag (isNegative val) NF

-- | Execute one of: AND | EOR | ORA
executeLogicalOp :: Instruction -> (Byte -> Byte -> Byte) -> Emulator ()
executeLogicalOp (Instruction _ (Operand opT _)) logOp = do
    result <- resolveOperand opT >>= applyARegister logOp
    updateFlag (isZero result)     ZF
    updateFlag (isNegative result) NF

-- | Execute ADC with DF clear
addBinary :: Instruction -> Emulator ()
addBinary (Instruction _ (Operand opT _)) = do
    aVal   <- getARegister
    adder  <- resolveOperand opT
    carry  <- boolToByte <$> isFlagSet CF
    result <- setARegister (aVal + adder + carry)
    updateFlag (isZero result)                ZF
    updateFlag (isNegative result)            NF
    updateFlag (isOverflow aVal adder result) OF
    updateFlag (isCarryAdd aVal result carry) CF

-- | Execute SBC with DF clear
subBinary :: Instruction -> Emulator ()
subBinary (Instruction _ (Operand opT _)) = do
    aVal   <- getARegister
    subber <- resolveOperand opT
    carry  <- boolToByte . not <$> isFlagSet CF
    result <- setARegister (aVal - subber - carry)
    updateFlag (isZero result)                 ZF
    updateFlag (isNegative result)             NF
    updateFlag (isOverflow aVal subber result) OF
    updateFlag (isCarrySub aVal subber)        CF

-- | Execute ADC with DF set
addDecimal :: Instruction -> Emulator ()
addDecimal (Instruction _ (Operand opT _)) = do
    aVal  <- getARegister
    adder <- resolveOperand opT
    carry <- boolToByte <$> isFlagSet CF
    let (bcdCarry, result) = computeBCDAdd aVal adder carry
    void $ setARegister result
    updateFlag (isZero result)     ZF
    updateFlag (isNegative result) NF  -- 6502 inspects BCD bit 7 even though it doesn't make much sense
    updateFlag False               OF  -- 6502 overflow flag is undefined for BCD
    updateFlag bcdCarry            CF

computeBCDAdd :: Byte -> Byte -> Byte -> (Bool, Byte)
computeBCDAdd acc adder carry =
        -- compute lower bits addition
    let onesDigit  = acc .&. 0xF + adder .&. 0xF + carry
        -- if we carry into upper bits, we add 0x6 to onesDigits to simulate wrapping
        onesDigit' = onesDigit + if onesDigit > 0x9 then 0x6 else 0x0
        onesCarry  = if (onesDigit .&. 0xF0) > 0 then 0x1 else 0x0
        -- compute upper bits
        tensDigit  = acc `shiftR` 4 + adder `shiftR` 4 + onesCarry
        -- same as lower bits
        tensDigit' = tensDigit + if tensDigit > 0x9 then 0x6 else 0x0
        bcdCarry   = tensDigit .&. 0xF0 > 0
    in  (bcdCarry, tensDigit' `shiftL` 4 + onesDigit')

-- | Execute SBC with DF set
subDecimal :: Instruction -> Emulator ()
subDecimal (Instruction _ (Operand opT _)) = do
    aVal   <- getARegister
    subber <- resolveOperand opT
    carry  <- boolToByte . not <$> isFlagSet CF
    let (bcdCarry, result) = computeBCDSub aVal subber carry
    void $ setARegister result
    updateFlag (isZero result)     ZF
    updateFlag (isNegative result) NF  -- 6502 inspects BCD bit 7 even though it doesn't make much sense
    updateFlag False               OF  -- 6502 overflow flag is undefined for BCD
    updateFlag bcdCarry            CF

computeBCDSub :: Byte -> Byte -> Byte -> (Bool, Byte)
computeBCDSub acc subber carry =
        -- compute lower bits subtraction
    let
        onesDigit  = acc .&. 0xF - subber .&. 0xF - carry
        -- if we carry into upper bits, we add 0x6 to onesDigits to simulate wrapping
        onesCarry  = (onesDigit .&. 0x10) /= 0
        onesDigit' = onesDigit - if onesCarry then 0x6 else 0x0
        -- compute upper bits
        tensDigit =
            acc `shiftR` 4 - subber `shiftR` 4 - if onesCarry then 0x1 else 0x0
        -- same as lower bits
        tensDigit' = tensDigit - if tensDigit .&. 0x10 /= 0 then 0x6 else 0x0
        bcdCarry   = acc <= subber
    in
        (bcdCarry, tensDigit' `shiftL` 4 + onesDigit')

-- | Execute one of: CMP | CPY | CPX
executeCompare :: Instruction -> Emulator ()
executeCompare (Instruction opCode (Operand opT _)) =
    executeCompare' opT =<< case getOpName opCode of
        CMP -> getARegister
        CPY -> getYRegister
        CPX -> getXRegister
        invalid ->
            error
                $  "Invalid instruction called with executeCompare: "
                <> show invalid

executeCompare' :: OperandType -> Byte -> Emulator ()
executeCompare' opT regVal = do
    cmpVal <- resolveOperand opT
    updateFlag (regVal >= cmpVal)             CF
    updateFlag (regVal == cmpVal)             ZF
    updateFlag (isNegative $ regVal - cmpVal) NF

executeIncOrDec :: (Byte -> Byte -> Byte) -> StoreLoc -> Emulator ()
executeIncOrDec binOp storeLoc = do
    result <- applyOperation storeLoc binOp 1
    updateFlag (isZero result)     ZF
    updateFlag (isNegative result) NF

executeShift :: (Byte -> Byte -> Byte) -> Operand -> Emulator (Byte, Byte)
executeShift shiftOp (Operand opT storeLoc) = do
    unshifted <- resolveOperand opT
    result    <- applyOperation storeLoc shiftOp 1
    return (unshifted, result)
