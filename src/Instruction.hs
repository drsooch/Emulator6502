module Instruction
    ( decodeOpCode
    , operandNum
    , getOpName
    ) where

import           Data.Word                      ( )
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
getOpName (OpCode _ opName _) = opName

-- take a Byte and turn it into an OpCode
decodeOpCode :: Byte -> OpCode
decodeOpCode b = case b of
  -- Add with carry
    0x61 -> OpCode b ADC IndirectX
    0x65 -> OpCode b ADC ZeroPage
    0x69 -> OpCode b ADC Immediate
    0x6D -> OpCode b ADC Absolute
    0x71 -> OpCode b ADC IndirectY
    0x75 -> OpCode b ADC ZeroPageX
    0x79 -> OpCode b ADC AbsoluteY
    0x7D -> OpCode b ADC AbsoluteX
    -- Logical And
    0x21 -> OpCode b AND IndirectX
    0x25 -> OpCode b AND ZeroPage
    0x29 -> OpCode b AND Immediate
    0x2D -> OpCode b AND Absolute
    0x31 -> OpCode b AND IndirectY
    0x35 -> OpCode b AND ZeroPageX
    0x39 -> OpCode b AND AbsoluteY
    0x3D -> OpCode b AND AbsoluteX
    -- Arithmetic Shift Left
    0x06 -> OpCode b ASL ZeroPage
    0x0A -> OpCode b ASL Accumulator
    0x0E -> OpCode b ASL Absolute
    0x16 -> OpCode b ASL ZeroPageX
    0x1E -> OpCode b ASL AbsoluteX
    -- Branch if Carry Clear
    0x90 -> OpCode b BCC Relative
    -- Branch if Carry Set
    0xB0 -> OpCode b BCS Relative
    -- Branch if Equal
    0xF0 -> OpCode b BEQ Relative
    -- Bit Test
    0x24 -> OpCode b BIT ZeroPage
    0x2C -> OpCode b BIT Absolute
    -- Branch if Minus
    0x30 -> OpCode b BMI Relative
    -- Branch if Not Equal
    0xD0 -> OpCode b BNE Relative
    -- Branch if Not Equal
    0x10 -> OpCode b BPL Relative
    -- Force Interrupt
    0x00 -> OpCode b BRK Implicit
    -- Branch if Overflow Clear
    0x50 -> OpCode b BVC Relative
    -- Branch if Overflow Set
    0x70 -> OpCode b BVS Relative
    -- Clear Carry Flag
    0x18 -> OpCode b CLC Implicit
    -- Clear Decimal Mode
    0xD8 -> OpCode b CLD Implicit
    -- Clear Interrupt Disable
    0x58 -> OpCode b CLI Implicit
    -- Clear Overflow Flag
    0xB8 -> OpCode b CLV Implicit
    -- Compare
    0xC1 -> OpCode b CMP IndirectX
    0xC5 -> OpCode b CMP ZeroPage
    0xC9 -> OpCode b CMP Immediate
    0xCD -> OpCode b CMP Absolute
    0xD1 -> OpCode b CMP IndirectY
    0xD5 -> OpCode b CMP ZeroPageX
    0xD9 -> OpCode b CMP AbsoluteY
    0xDD -> OpCode b CMP AbsoluteX
    -- Compare X Register
    0xE0 -> OpCode b CPX Immediate
    0xE4 -> OpCode b CPX ZeroPage
    0xEC -> OpCode b CPX Absolute
    -- Compare Y Register
    0xC0 -> OpCode b CPY Immediate
    0xC4 -> OpCode b CPY ZeroPage
    0xCC -> OpCode b CPY Absolute
    -- Decrement Memory
    0xC6 -> OpCode b DEC ZeroPage
    0xCE -> OpCode b DEC Absolute
    0xD6 -> OpCode b DEC ZeroPageX
    0xDE -> OpCode b DEC AbsoluteX
    -- Decrement X Register
    0xCA -> OpCode b DEX Implicit
    -- Decrement Y Register
    0x88 -> OpCode b DEY Implicit
    -- Exclusive Or
    0x41 -> OpCode b EOR IndirectX
    0x45 -> OpCode b EOR ZeroPage
    0x49 -> OpCode b EOR Immediate
    0x4D -> OpCode b EOR Absolute
    0x51 -> OpCode b EOR IndirectY
    0x55 -> OpCode b EOR ZeroPageX
    0x59 -> OpCode b EOR AbsoluteY
    0x5D -> OpCode b EOR AbsoluteX
    -- Decrement Memory
    0xE6 -> OpCode b INC ZeroPage
    0xEE -> OpCode b INC Absolute
    0xF6 -> OpCode b INC ZeroPageX
    0xFE -> OpCode b INC AbsoluteX
    -- Decrement X Register
    0xE8 -> OpCode b INX Implicit
    -- Decrement Y Register
    0xC8 -> OpCode b INY Implicit
    -- Jump
    0x4C -> OpCode b JMP Absolute
    0x6C -> OpCode b JMP Indirect
    -- Jump to Subroutine
    0x20 -> OpCode b JSR Absolute
    -- Load Accumulator
    0xA1 -> OpCode b LDA IndirectX
    0xA5 -> OpCode b LDA ZeroPage
    0xA9 -> OpCode b LDA Immediate
    0xAD -> OpCode b LDA Absolute
    0xB1 -> OpCode b LDA IndirectY
    0xB5 -> OpCode b LDA ZeroPageX
    0xB9 -> OpCode b LDA AbsoluteY
    0xBD -> OpCode b LDA AbsoluteX
    -- Load X Register
    0xA2 -> OpCode b LDX Immediate
    0xA6 -> OpCode b LDX ZeroPage
    0xAE -> OpCode b LDX Absolute
    0xB6 -> OpCode b LDX ZeroPageY
    0xBE -> OpCode b LDX AbsoluteY
    -- Load Y Register
    0xA0 -> OpCode b LDY Immediate
    0xA4 -> OpCode b LDY ZeroPage
    0xAC -> OpCode b LDY Absolute
    0xB4 -> OpCode b LDY ZeroPageX
    0xBC -> OpCode b LDY AbsoluteX
    -- Logical Shift Right
    0x46 -> OpCode b LSR ZeroPage
    0x4A -> OpCode b LSR Accumulator
    0x4E -> OpCode b LSR Absolute
    0x56 -> OpCode b LSR ZeroPageX
    0x5E -> OpCode b LSR AbsoluteX
    -- No Operation
    0x04 -> OpCode b NOP Implicit -- doesn't matter location
    -- Logical Inclusive Or
    0x01 -> OpCode b ORA IndirectX
    0x05 -> OpCode b ORA ZeroPage
    0x09 -> OpCode b ORA Immediate
    0x0D -> OpCode b ORA Absolute
    0x11 -> OpCode b ORA IndirectY
    0x15 -> OpCode b ORA ZeroPageX
    0x19 -> OpCode b ORA AbsoluteY
    0x1D -> OpCode b ORA AbsoluteX
    -- Push Accumulator
    0x48 -> OpCode b PHA Implicit
    -- Push Processor Status
    0x08 -> OpCode b PHP Implicit
    -- Pull Accumulator
    0x68 -> OpCode b PLA Implicit
    -- Pull Processor Status
    0x28 -> OpCode b PLP Implicit
    -- Rotate Left
    0x26 -> OpCode b ROL ZeroPage
    0x2A -> OpCode b ROL Accumulator
    0x2E -> OpCode b ROL Absolute
    0x36 -> OpCode b ROL ZeroPageX
    0x3E -> OpCode b ROL AbsoluteX
    -- Rotate Right
    0x66 -> OpCode b ROR ZeroPage
    0x6A -> OpCode b ROR Accumulator
    0x6E -> OpCode b ROR Absolute
    0x76 -> OpCode b ROR ZeroPageX
    0x7E -> OpCode b ROR AbsoluteX
    -- Return from Interrupt
    0x40 -> OpCode b RTI Implicit
    -- Return from Subroutine
    0x60 -> OpCode b RTS Implicit
    -- Subtract with Carry
    0xE1 -> OpCode b SBC IndirectX
    0xE5 -> OpCode b SBC ZeroPage
    0xE9 -> OpCode b SBC Immediate
    0xED -> OpCode b SBC Absolute
    0xF1 -> OpCode b SBC IndirectY
    0xF5 -> OpCode b SBC ZeroPageX
    0xF9 -> OpCode b SBC AbsoluteY
    0xFD -> OpCode b SBC AbsoluteX
    -- Set Carry Flag
    0x38 -> OpCode b SEC Implicit
    -- Set Decimal Flag
    0xF8 -> OpCode b SED Implicit
    -- Set Interrupt Disable
    0x78 -> OpCode b SEI Implicit
    -- Store Accumulator
    0x81 -> OpCode b STA IndirectX
    0x85 -> OpCode b STA ZeroPage
    0x8D -> OpCode b STA Absolute
    0x91 -> OpCode b STA IndirectY
    0x95 -> OpCode b STA ZeroPageX
    0x99 -> OpCode b STA AbsoluteY
    0x9D -> OpCode b STA AbsoluteX
    -- Store X Register
    0x86 -> OpCode b STX ZeroPage
    0x8E -> OpCode b STX Absolute
    0x96 -> OpCode b STX ZeroPageY
    -- Store Y Register
    0x84 -> OpCode b STY ZeroPage
    0x8C -> OpCode b STY Absolute
    0x94 -> OpCode b STY ZeroPageX
    -- Transfer Accumulator to X
    0xAA -> OpCode b TAX Implicit
    -- Transfer Accumulator to Y
    0xA8 -> OpCode b TAY Implicit
    -- Transfer Stack Pointer to X
    0xBA -> OpCode b TSX Implicit
    -- Transfer X to Accumulator
    0x8A -> OpCode b TXA Implicit
    -- Transfer X to Stack Pointer
    0x9A -> OpCode b TXS Implicit
    -- Transfer Y to Accumulator
    0x98 -> OpCode b TYA Implicit
    _    -> undefined
