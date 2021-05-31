{-# LANGUAGE RankNTypes #-}
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
import           Decode
import           Flags
import           Instruction
import           Logging
import           Memory
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
executeInstruction inst@(Instruction (OpCode opName _) operand) =
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
            opVal <- resolveOperand operand
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
        INC -> executeIncOrDec (+) operand
        INX -> executeIncOrDec (+) operand
        INY -> executeIncOrDec (+) operand
        DEC -> executeIncOrDec (-) operand
        DEX -> executeIncOrDec (-) operand
        DEY -> executeIncOrDec (-) operand
        -- Shift Ops
        ASL -> do
            (unshift, result) <- executeShift shiftL operand
            updateFlag (isNegative unshift) CF -- Carry Flag is set to bit 7 (same as isNegative)
            updateFlag (isZero result)      ZF
            updateFlag (isNegative result)  NF
        LSR -> do
            (unshift, result) <- executeShift shiftR operand
            updateFlag (testBit unshift 0) CF -- CF is equal to bit Zero of unshifted
            updateFlag (isZero result)     ZF
            updateFlag (isNegative result) NF -- this should always be False
        ROL -> do
            unrotated <- resolveOperand operand
            carry     <- bool 0 1 <$> isFlagSet CF
            -- rotFn ignores second value as applyOperation is a binaryOp
            -- could also ignore first value also but whatever
            let rotFn = \unrot _ -> unrot `shiftL` 1 .|. carry
            result <- applyOperation operand rotFn 0
            updateFlag (isNegative unrotated) CF
            updateFlag (isZero result)        ZF
            updateFlag (isNegative result)    NF
        ROR -> do
            unrotated <- resolveOperand operand
            -- if carry is set we will set bit 7
            carry     <- bool 0 0x80 <$> isFlagSet CF
            -- see above
            let rotFn = \unrot _ -> unrot `shiftR` 1 .|. carry
            result <- applyOperation operand rotFn 0
            updateFlag (testBit unrotated 0) CF
            updateFlag (isZero result)       ZF
            updateFlag (isNegative result)   NF
        -- Jump Ops
        JMP ->
            -- the StoreLoc of Operand holds jump point
            -- requires a dummy byte val
            -- yuck.
            storeValue operand 0
        JSR ->
            -- push current PC onto stack (minus one for some reason)
            -- TODO: figure out why
            getProgramCounter >>= pushStackAddress . (-) 1
               -- see JMP not sure if there's even a point to adding and subtracting 1
        RTS -> popStackAddress >>= setProgramCounter . (+) 1
        -- Branch Ops
        BCC -> isFlagSet CF >>= \flag -> unless flag $ executeBranch operand
        BCS -> isFlagSet CF >>= \flag -> when flag $ executeBranch operand
        BEQ -> isFlagSet ZF >>= \flag -> when flag $ executeBranch operand
        BMI -> isFlagSet NF >>= \flag -> when flag $ executeBranch operand
        BNE -> isFlagSet ZF >>= \flag -> unless flag $ executeBranch operand
        BPL -> isFlagSet NF >>= \flag -> unless flag $ executeBranch operand
        BVC -> isFlagSet OF >>= \flag -> unless flag $ executeBranch operand
        BVS -> isFlagSet OF >>= \flag -> when flag $ executeBranch operand
        -- Status Flag Ops
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
executeLoad (Instruction (OpCode opName _) ops) = do
    loadVal <- resolveOperand ops
    let setReg = case opName of
            LDX -> setXRegister
            LDY -> setYRegister
            LDA -> setARegister
            x   -> error $ show x
    _ <- setReg loadVal
    updateFlag (isZero loadVal)     ZF
    updateFlag (isNegative loadVal) NF

-- | Execute one of: STA | STX | STY
executeStore :: Instruction -> Emulator ()
executeStore (Instruction (OpCode opName _) operand) = do
    regVal <- case opName of
        STA -> getARegister
        STX -> getXRegister
        STY -> getYRegister
        x   -> error $ show x
    case getStoreLoc operand of
        (MemorySL addr) -> setMemory addr [regVal]
        _               -> error "Invalid store Location executeStore"

-- | Execute one of: TAX | TAY | TXA | TYA
executeTransfer :: Instruction -> Emulator ()
executeTransfer (Instruction (OpCode opName _) _) = do
    let
        (fromReg, toReg) = case opName of
            TAX -> (getARegister, setXRegister)
            TAY -> (getARegister, setYRegister)
            TXA -> (getXRegister, setARegister)
            TYA -> (getYRegister, setARegister)
            invalid ->
                error
                    $  "Invalid instruction in executeTransfer: "
                    <> show invalid
    val <- fromReg
    updateFlag (isZero val)     ZF
    updateFlag (isNegative val) NF
    void $ toReg val

-- | Execute one of: AND | EOR | ORA
executeLogicalOp :: Instruction -> (Byte -> Byte -> Byte) -> Emulator ()
executeLogicalOp (Instruction _ opt) logOp = do
    result <- resolveOperand opt >>= applyARegister logOp
    updateFlag (isZero result)     ZF
    updateFlag (isNegative result) NF

-- | Execute ADC with DF clear
addBinary :: Instruction -> Emulator ()
addBinary (Instruction _ opt) = do
    aVal   <- getARegister
    adder  <- resolveOperand opt
    carry  <- boolToByte <$> isFlagSet CF
    result <- setARegister (aVal + adder + carry)
    updateFlag (isZero result)                ZF
    updateFlag (isNegative result)            NF
    updateFlag (isOverflow aVal adder result) OF
    updateFlag (isCarryAdd aVal result carry) CF

-- | Execute SBC with DF clear
subBinary :: Instruction -> Emulator ()
subBinary (Instruction _ opt) = do
    aVal   <- getARegister
    subber <- resolveOperand opt
    carry  <- boolToByte . not <$> isFlagSet CF
    result <- setARegister (aVal - subber - carry)
    updateFlag (isZero result)                 ZF
    updateFlag (isNegative result)             NF
    updateFlag (isOverflow aVal subber result) OF
    updateFlag (isCarrySub aVal subber)        CF

-- | Execute ADC with DF set
addDecimal :: Instruction -> Emulator ()
addDecimal (Instruction _ opt) = do
    aVal  <- getARegister
    adder <- resolveOperand opt
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
subDecimal (Instruction _ opt) = do
    aVal   <- getARegister
    subber <- resolveOperand opt
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
executeCompare (Instruction opCode opt) =
    executeCompare' opt =<< case getOpName opCode of
        CMP -> getARegister
        CPY -> getYRegister
        CPX -> getXRegister
        invalid ->
            error
                $  "Invalid instruction called with executeCompare: "
                <> show invalid

executeCompare' :: Operand -> Byte -> Emulator ()
executeCompare' opt regVal = do
    cmpVal <- resolveOperand opt
    updateFlag (regVal >= cmpVal)             CF
    updateFlag (regVal == cmpVal)             ZF
    updateFlag (isNegative $ regVal - cmpVal) NF

executeIncOrDec :: (Byte -> Byte -> Byte) -> Operand -> Emulator ()
executeIncOrDec binOp operand = do
    result <- applyOperation operand binOp 1
    updateFlag (isZero result)     ZF
    updateFlag (isNegative result) NF

executeShift :: (Byte -> Byte -> Byte) -> Operand -> Emulator (Byte, Byte)
executeShift shiftOp operand = do
    unshifted <- resolveOperand operand
    result    <- applyOperation operand shiftOp 1
    return (unshifted, result)

executeBranch :: Operand -> Emulator ()
executeBranch operand = do
    offset <- resolveOperand operand
    offsetProgramCounter offset
