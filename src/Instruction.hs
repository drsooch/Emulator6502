module Instruction
    ( decodeOpCode
    , operandNum
    , getOpName
    , getStoreLoc
    , decodeStoreLoc
    ) where

import           Types


-- number of Bytes to get for addressing
operandNum :: AddressType -> Int
operandNum Accumulator = 0
operandNum Absolute    = 2
operandNum AbsoluteX   = 2
operandNum AbsoluteY   = 2
operandNum Immediate   = 1
operandNum Implicit    = 0
operandNum Indirect    = 2
operandNum IndirectX   = 1
operandNum IndirectY   = 1
operandNum Relative    = 1
operandNum ZeroPage    = 1
operandNum ZeroPageX   = 1
operandNum ZeroPageY   = 1

-- get the OpName from an OpCode
getOpName :: OpCode -> OpName
getOpName (OpCode opName _) = opName

getStoreLoc :: Operand -> StoreLoc
getStoreLoc (Operand _ loc) = loc

decodeStoreLoc :: OpCode -> OperandType -> AddressType -> StoreLoc
decodeStoreLoc opc opt mode = case (getOpName opc, mode, opt) of
    (LDA, _          , _             ) -> RegisterSL AReg
    (LDX, _          , _             ) -> RegisterSL XReg
    (LDY, _          , _             ) -> RegisterSL YReg
    (STA, _          , OpTMemory addr) -> MemorySL addr
    (STX, _          , OpTMemory addr) -> MemorySL addr
    (STY, _          , OpTMemory addr) -> MemorySL addr
    (TAX, _          , _             ) -> RegisterSL XReg
    (TAY, _          , _             ) -> RegisterSL YReg
    (TXA, _          , _             ) -> RegisterSL AReg
    (TYA, _          , _             ) -> RegisterSL AReg
    (TSX, _          , _             ) -> RegisterSL XReg
    (TXS, _          , _             ) -> StackPointerSL
    (PHP, _          , _             ) -> NoStore
    (PHA, _          , _             ) -> NoStore
    (PLA, _          , _             ) -> RegisterSL AReg
    (PLP, _          , _             ) -> NoStore
    (AND, _          , _             ) -> RegisterSL AReg
    (EOR, _          , _             ) -> RegisterSL AReg
    (ORA, _          , _             ) -> RegisterSL AReg
    (BIT, _          , _             ) -> NoStore
    (ADC, _          , _             ) -> RegisterSL AReg
    (SBC, _          , _             ) -> RegisterSL AReg
    (CMP, _          , _             ) -> NoStore
    (CPX, _          , _             ) -> NoStore
    (CPY, _          , _             ) -> NoStore
    (INC, _          , OpTMemory addr) -> MemorySL addr
    (INX, _          , _             ) -> RegisterSL XReg
    (INY, _          , _             ) -> RegisterSL YReg
    (DEC, _          , OpTMemory addr) -> MemorySL addr
    (DEX, _          , _             ) -> RegisterSL XReg
    (DEY, _          , _             ) -> RegisterSL YReg
    (ASL, Accumulator, _             ) -> RegisterSL AReg
    (ASL, _          , OpTMemory addr) -> MemorySL addr
    (LSR, Accumulator, _             ) -> RegisterSL AReg
    (LSR, _          , OpTMemory addr) -> MemorySL addr
    (ROL, Accumulator, _             ) -> RegisterSL AReg
    (ROL, _          , OpTMemory addr) -> MemorySL addr
    (ROR, Accumulator, _             ) -> RegisterSL AReg
    (ROR, _          , OpTMemory addr) -> MemorySL addr
    (JMP, _          , OpTMemory addr) -> ProgramCounterSL addr
    (JSR, _          , _             ) -> NoStore
    (RTS, _          , _             ) -> NoStore
    (BCC, _          , _             ) -> NoStore
    (BCS, _          , _             ) -> NoStore
    (BEQ, _          , _             ) -> NoStore
    (BMI, _          , _             ) -> NoStore
    (BNE, _          , _             ) -> NoStore
    (BPL, _          , _             ) -> NoStore
    (BVC, _          , _             ) -> NoStore
    (BVS, _          , _             ) -> NoStore
    (CLC, _          , _             ) -> NoStore
    (CLD, _          , _             ) -> NoStore
    (CLI, _          , _             ) -> NoStore
    (CLV, _          , _             ) -> NoStore
    (SEC, _          , _             ) -> NoStore
    (SED, _          , _             ) -> NoStore
    (SEI, _          , _             ) -> NoStore
    (NOP, _          , _             ) -> NoStore
    (BRK, _          , _             ) -> NoStore
    (RTI, _          , _             ) -> NoStore
    (_  , _          , _             ) -> error "Invalid Instruction"


-- take a Byte and turn it into an OpCode
decodeOpCode :: Byte -> OpCode
decodeOpCode b = case b of
  -- Add with carry
    0x61 -> OpCode ADC IndirectX
    0x65 -> OpCode ADC ZeroPage
    0x69 -> OpCode ADC Immediate
    0x6D -> OpCode ADC Absolute
    0x71 -> OpCode ADC IndirectY
    0x75 -> OpCode ADC ZeroPageX
    0x79 -> OpCode ADC AbsoluteY
    0x7D -> OpCode ADC AbsoluteX
    -- Logical And
    0x21 -> OpCode AND IndirectX
    0x25 -> OpCode AND ZeroPage
    0x29 -> OpCode AND Immediate
    0x2D -> OpCode AND Absolute
    0x31 -> OpCode AND IndirectY
    0x35 -> OpCode AND ZeroPageX
    0x39 -> OpCode AND AbsoluteY
    0x3D -> OpCode AND AbsoluteX
    -- Arithmetic Shift Left
    0x06 -> OpCode ASL ZeroPage
    0x0A -> OpCode ASL Accumulator
    0x0E -> OpCode ASL Absolute
    0x16 -> OpCode ASL ZeroPageX
    0x1E -> OpCode ASL AbsoluteX
    -- Branch if Carry Clear
    0x90 -> OpCode BCC Relative
    -- Branch if Carry Set
    0xB0 -> OpCode BCS Relative
    -- Branch if Equal
    0xF0 -> OpCode BEQ Relative
    -- Bit Test
    0x24 -> OpCode BIT ZeroPage
    0x2C -> OpCode BIT Absolute
    -- Branch if Minus
    0x30 -> OpCode BMI Relative
    -- Branch if Not Equal
    0xD0 -> OpCode BNE Relative
    -- Branch if Not Equal
    0x10 -> OpCode BPL Relative
    -- Force Interrupt
    0x00 -> OpCode BRK Implicit
    -- Branch if Overflow Clear
    0x50 -> OpCode BVC Relative
    -- Branch if Overflow Set
    0x70 -> OpCode BVS Relative
    -- Clear Carry Flag
    0x18 -> OpCode CLC Implicit
    -- Clear Decimal Mode
    0xD8 -> OpCode CLD Implicit
    -- Clear Interrupt Disable
    0x58 -> OpCode CLI Implicit
    -- Clear Overflow Flag
    0xB8 -> OpCode CLV Implicit
    -- Compare
    0xC1 -> OpCode CMP IndirectX
    0xC5 -> OpCode CMP ZeroPage
    0xC9 -> OpCode CMP Immediate
    0xCD -> OpCode CMP Absolute
    0xD1 -> OpCode CMP IndirectY
    0xD5 -> OpCode CMP ZeroPageX
    0xD9 -> OpCode CMP AbsoluteY
    0xDD -> OpCode CMP AbsoluteX
    -- Compare X Register
    0xE0 -> OpCode CPX Immediate
    0xE4 -> OpCode CPX ZeroPage
    0xEC -> OpCode CPX Absolute
    -- Compare Y Register
    0xC0 -> OpCode CPY Immediate
    0xC4 -> OpCode CPY ZeroPage
    0xCC -> OpCode CPY Absolute
    -- Decrement Memory
    0xC6 -> OpCode DEC ZeroPage
    0xCE -> OpCode DEC Absolute
    0xD6 -> OpCode DEC ZeroPageX
    0xDE -> OpCode DEC AbsoluteX
    -- Decrement X Register
    0xCA -> OpCode DEX Implicit
    -- Decrement Y Register
    0x88 -> OpCode DEY Implicit
    -- Exclusive Or
    0x41 -> OpCode EOR IndirectX
    0x45 -> OpCode EOR ZeroPage
    0x49 -> OpCode EOR Immediate
    0x4D -> OpCode EOR Absolute
    0x51 -> OpCode EOR IndirectY
    0x55 -> OpCode EOR ZeroPageX
    0x59 -> OpCode EOR AbsoluteY
    0x5D -> OpCode EOR AbsoluteX
    -- Decrement Memory
    0xE6 -> OpCode INC ZeroPage
    0xEE -> OpCode INC Absolute
    0xF6 -> OpCode INC ZeroPageX
    0xFE -> OpCode INC AbsoluteX
    -- Decrement X Register
    0xE8 -> OpCode INX Implicit
    -- Decrement Y Register
    0xC8 -> OpCode INY Implicit
    -- Jump
    0x4C -> OpCode JMP Absolute
    0x6C -> OpCode JMP Indirect
    -- Jump to Subroutine
    0x20 -> OpCode JSR Absolute
    -- Load Accumulator
    0xA1 -> OpCode LDA IndirectX
    0xA5 -> OpCode LDA ZeroPage
    0xA9 -> OpCode LDA Immediate
    0xAD -> OpCode LDA Absolute
    0xB1 -> OpCode LDA IndirectY
    0xB5 -> OpCode LDA ZeroPageX
    0xB9 -> OpCode LDA AbsoluteY
    0xBD -> OpCode LDA AbsoluteX
    -- Load X Register
    0xA2 -> OpCode LDX Immediate
    0xA6 -> OpCode LDX ZeroPage
    0xAE -> OpCode LDX Absolute
    0xB6 -> OpCode LDX ZeroPageY
    0xBE -> OpCode LDX AbsoluteY
    -- Load Y Register
    0xA0 -> OpCode LDY Immediate
    0xA4 -> OpCode LDY ZeroPage
    0xAC -> OpCode LDY Absolute
    0xB4 -> OpCode LDY ZeroPageX
    0xBC -> OpCode LDY AbsoluteX
    -- Logical Shift Right
    0x46 -> OpCode LSR ZeroPage
    0x4A -> OpCode LSR Accumulator
    0x4E -> OpCode LSR Absolute
    0x56 -> OpCode LSR ZeroPageX
    0x5E -> OpCode LSR AbsoluteX
    -- No Operation
    0x04 -> OpCode NOP Implicit -- doesn't matter location
    -- Logical Inclusive Or
    0x01 -> OpCode ORA IndirectX
    0x05 -> OpCode ORA ZeroPage
    0x09 -> OpCode ORA Immediate
    0x0D -> OpCode ORA Absolute
    0x11 -> OpCode ORA IndirectY
    0x15 -> OpCode ORA ZeroPageX
    0x19 -> OpCode ORA AbsoluteY
    0x1D -> OpCode ORA AbsoluteX
    -- Push Accumulator
    0x48 -> OpCode PHA Implicit
    -- Push Processor Status
    0x08 -> OpCode PHP Implicit
    -- Pull Accumulator
    0x68 -> OpCode PLA Implicit
    -- Pull Processor Status
    0x28 -> OpCode PLP Implicit
    -- Rotate Left
    0x26 -> OpCode ROL ZeroPage
    0x2A -> OpCode ROL Accumulator
    0x2E -> OpCode ROL Absolute
    0x36 -> OpCode ROL ZeroPageX
    0x3E -> OpCode ROL AbsoluteX
    -- Rotate Right
    0x66 -> OpCode ROR ZeroPage
    0x6A -> OpCode ROR Accumulator
    0x6E -> OpCode ROR Absolute
    0x76 -> OpCode ROR ZeroPageX
    0x7E -> OpCode ROR AbsoluteX
    -- Return from Interrupt
    0x40 -> OpCode RTI Implicit
    -- Return from Subroutine
    0x60 -> OpCode RTS Implicit
    -- Subtract with Carry
    0xE1 -> OpCode SBC IndirectX
    0xE5 -> OpCode SBC ZeroPage
    0xE9 -> OpCode SBC Immediate
    0xED -> OpCode SBC Absolute
    0xF1 -> OpCode SBC IndirectY
    0xF5 -> OpCode SBC ZeroPageX
    0xF9 -> OpCode SBC AbsoluteY
    0xFD -> OpCode SBC AbsoluteX
    -- Set Carry Flag
    0x38 -> OpCode SEC Implicit
    -- Set Decimal Flag
    0xF8 -> OpCode SED Implicit
    -- Set Interrupt Disable
    0x78 -> OpCode SEI Implicit
    -- Store Accumulator
    0x81 -> OpCode STA IndirectX
    0x85 -> OpCode STA ZeroPage
    0x8D -> OpCode STA Absolute
    0x91 -> OpCode STA IndirectY
    0x95 -> OpCode STA ZeroPageX
    0x99 -> OpCode STA AbsoluteY
    0x9D -> OpCode STA AbsoluteX
    -- Store X Register
    0x86 -> OpCode STX ZeroPage
    0x8E -> OpCode STX Absolute
    0x96 -> OpCode STX ZeroPageY
    -- Store Y Register
    0x84 -> OpCode STY ZeroPage
    0x8C -> OpCode STY Absolute
    0x94 -> OpCode STY ZeroPageX
    -- Transfer Accumulator to X
    0xAA -> OpCode TAX Implicit
    -- Transfer Accumulator to Y
    0xA8 -> OpCode TAY Implicit
    -- Transfer Stack Pointer to X
    0xBA -> OpCode TSX Implicit
    -- Transfer X to Accumulator
    0x8A -> OpCode TXA Implicit
    -- Transfer X to Stack Pointer
    0x9A -> OpCode TXS Implicit
    -- Transfer Y to Accumulator
    0x98 -> OpCode TYA Implicit
    _    -> undefined
