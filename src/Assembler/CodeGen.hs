-- | Generate an Executable file

module Assembler.CodeGen where

import           Assembler.Error
import           Assembler.Types
import           Data.Foldable                  ( foldl' )
import qualified Data.Map.Strict               as M
import           Data.Monoid                    ( Sum(..)
                                                , getSum
                                                )
import           Data.Word                      ( Word16 )
import           Lens.Micro.Mtl                 ( (%=)
                                                , use
                                                )
import           Types


-- FIXME: AsmUnknown and AsmUnknownImm need to be added

-- construct the data section of the Executable
-- reserving byte space and incrementing label offsets
emitDataBytes :: Assembler [ByteType]
emitDataBytes = undefined

convertDataToByte :: AsmStatement -> Assembler ByteType
convertDataToByte = undefined

emitCodeBytes :: Assembler [ByteType]
emitCodeBytes = undefined

getProgramStart :: Assembler (Maybe Address)
getProgramStart = undefined -- do
    -- gets (progStartIdent . asmTree)
        -- >>= \(Just (StmtProgramLocation val)) -> pure $ Just $ getAsmNumericVal val

codeBlockSize :: Assembler ()
codeBlockSize = do
    codeBlocks' <- use $ #asmTree . #codeBlocks
    #bindings . #codeLabels %= flip codeBlockSize' codeBlocks'

codeBlockSize' :: CodeLabels -> [CodeBlock] -> CodeLabels
codeBlockSize' =
    foldl' (\binds' CodeBlock {..} -> M.insert blockLabel (codeStatementSize statements) binds')

codeStatementSize :: [CodeStatement] -> Word16
codeStatementSize =
    getSum . foldMap (\CodeStatement {..} -> Sum $ 1 + addressTypeToSize addressType)

-- convert an address type to number of bytes to encode
addressTypeToSize :: AsmAddressType -> Word16
addressTypeToSize = \case
    AsmAccumulator        -> 0
    AsmAbsolute _ _       -> 2
    AsmImmediate _        -> 1
    AsmImplicit           -> 0
    AsmIndirect _ Nothing -> 2
    AsmIndirect _ _       -> 1
    AsmRelative _         -> 1
    AsmZeroPage _ _       -> 1
    AsmUnknown    _       -> 2
    AsmUnknownImm _       -> 1

instructionToByte :: OpName -> AsmAddressType -> Assembler [ByteType]
instructionToByte opn addrT = pure $ case opn of
    LDA -> ldaToByte addrT : []
    LDX -> ldxToByte addrT : []
    LDY -> ldyToByte addrT : []
    STA -> staToByte addrT : []
    STX -> stxToByte addrT : []
    STY -> styToByte addrT : []
    TAX -> [Literal 0xAA]
    TAY -> [Literal 0xA8]
    TXA -> [Literal 0x8A]
    TYA -> [Literal 0x98]
    TSX -> [Literal 0xBA]
    TXS -> [Literal 0x9A]
    PHA -> [Literal 0x48]
    PHP -> [Literal 0x08]
    PLA -> [Literal 0x68]
    PLP -> [Literal 0x28]
    AND -> andToByte addrT : []
    EOR -> eorToByte addrT : []
    ORA -> oraToByte addrT : []
    BIT -> bitToByte addrT : []
    ADC -> adcToByte addrT : []
    SBC -> sbcToByte addrT : []
    CMP -> cmpToByte addrT : []
    CPX -> cpxToByte addrT : []
    CPY -> cpyToByte addrT : []
    INC -> incToByte addrT : []
    INX -> [Literal 0xE8]
    INY -> [Literal 0xC8]
    DEC -> decToByte addrT : []
    DEX -> [Literal 0xCA]
    DEY -> [Literal 0x88]
    ASL -> aslToByte addrT : []
    LSR -> lsrToByte addrT : []
    ROL -> rolToByte addrT : []
    ROR -> rorToByte addrT : []
    JMP -> jmpToByte addrT : []
    JSR -> [Literal 0x20]
    RTS -> [Literal 0x60]
    BCC -> [Literal 0x90]
    BCS -> [Literal 0xB0]
    BEQ -> [Literal 0xF0]
    BMI -> [Literal 0x30]
    BNE -> [Literal 0xD0]
    BPL -> [Literal 0x10]
    BVC -> [Literal 0x50]
    BVS -> [Literal 0x70]
    CLC -> [Literal 0x18]
    CLD -> [Literal 0xD8]
    CLI -> [Literal 0x58]
    CLV -> [Literal 0xB8]
    SEC -> [Literal 0x38]
    SED -> [Literal 0xF8]
    SEI -> [Literal 0x78]
    BRK -> [Literal 0x00]
    NOP -> [Literal 0xEA]
    RTI -> [Literal 0x40]

operandToBytes :: AsmNumeric -> [ByteType]
operandToBytes = \case
    HexLiteral       num -> undefined
    HexImmediate     num -> undefined
    BinaryLiteral    num -> undefined
    BinaryImmediate  num -> undefined
    DecimalLiteral   num -> undefined
    DecimalImmediate num -> undefined

ldaToByte :: AsmAddressType -> ByteType
ldaToByte = \case
    AsmImmediate _            -> Literal 0xA9
    AsmZeroPage _ (Just AsmX) -> Literal 0xB5
    AsmZeroPage _ Nothing     -> Literal 0xA5
    AsmAbsolute _ (Just AsmX) -> Literal 0xBD
    AsmAbsolute _ (Just AsmY) -> Literal 0xB9
    AsmAbsolute _ Nothing     -> Literal 0xAD
    AsmIndirect _ (Just AsmX) -> Literal 0xA1
    AsmIndirect _ (Just AsmY) -> Literal 0xB1
    AsmUnknown _              -> undefined
    _                         -> assertNotReached

ldxToByte :: AsmAddressType -> ByteType
ldxToByte = \case
    AsmImmediate _            -> Literal 0xA2
    AsmZeroPage _ (Just AsmY) -> Literal 0xB6
    AsmZeroPage _ Nothing     -> Literal 0xA6
    AsmAbsolute _ (Just AsmY) -> Literal 0xBE
    AsmAbsolute _ Nothing     -> Literal 0xAE
    _                         -> assertNotReached

ldyToByte :: AsmAddressType -> ByteType
ldyToByte = \case
    AsmImmediate _            -> Literal 0xA0
    AsmZeroPage _ (Just AsmX) -> Literal 0xB4
    AsmZeroPage _ Nothing     -> Literal 0xA4
    AsmAbsolute _ (Just AsmX) -> Literal 0xBC
    AsmAbsolute _ Nothing     -> Literal 0xAC
    _                         -> assertNotReached

staToByte :: AsmAddressType -> ByteType
staToByte = \case
    AsmZeroPage _ (Just AsmX) -> Literal 0x95
    AsmZeroPage _ Nothing     -> Literal 0x85
    AsmAbsolute _ (Just AsmX) -> Literal 0x9D
    AsmAbsolute _ (Just AsmY) -> Literal 0x99
    AsmAbsolute _ Nothing     -> Literal 0x8D
    AsmIndirect _ (Just AsmX) -> Literal 0x81
    AsmIndirect _ (Just AsmY) -> Literal 0x91
    _                         -> assertNotReached

stxToByte :: AsmAddressType -> ByteType
stxToByte = \case
    AsmZeroPage _ (Just AsmY) -> Literal 0x96
    AsmZeroPage _ Nothing     -> Literal 0x86
    AsmAbsolute _ Nothing     -> Literal 0x8E
    _                         -> assertNotReached

styToByte :: AsmAddressType -> ByteType
styToByte = \case
    AsmZeroPage _ (Just AsmX) -> Literal 0x94
    AsmZeroPage _ Nothing     -> Literal 0x84
    AsmAbsolute _ Nothing     -> Literal 0x8C
    _                         -> assertNotReached

andToByte :: AsmAddressType -> ByteType
andToByte = \case
    AsmImmediate _            -> Literal 0x29
    AsmZeroPage _ (Just AsmX) -> Literal 0x35
    AsmZeroPage _ Nothing     -> Literal 0x25
    AsmAbsolute _ (Just AsmX) -> Literal 0x3D
    AsmAbsolute _ (Just AsmY) -> Literal 0x39
    AsmAbsolute _ Nothing     -> Literal 0x2D
    AsmIndirect _ (Just AsmX) -> Literal 0x21
    AsmIndirect _ (Just AsmY) -> Literal 0x31
    _                         -> assertNotReached

eorToByte :: AsmAddressType -> ByteType
eorToByte = \case
    AsmImmediate _            -> Literal 0x49
    AsmZeroPage _ (Just AsmX) -> Literal 0x55
    AsmZeroPage _ Nothing     -> Literal 0x45
    AsmAbsolute _ (Just AsmX) -> Literal 0x5D
    AsmAbsolute _ (Just AsmY) -> Literal 0x59
    AsmAbsolute _ Nothing     -> Literal 0x4D
    AsmIndirect _ (Just AsmX) -> Literal 0x41
    AsmIndirect _ (Just AsmY) -> Literal 0x51
    _                         -> assertNotReached

oraToByte :: AsmAddressType -> ByteType
oraToByte = \case
    AsmImmediate _            -> Literal 0x09
    AsmZeroPage _ (Just AsmX) -> Literal 0x15
    AsmZeroPage _ Nothing     -> Literal 0x05
    AsmAbsolute _ (Just AsmX) -> Literal 0x1D
    AsmAbsolute _ (Just AsmY) -> Literal 0x19
    AsmAbsolute _ Nothing     -> Literal 0x0D
    AsmIndirect _ (Just AsmX) -> Literal 0x01
    AsmIndirect _ (Just AsmY) -> Literal 0x11
    _                         -> assertNotReached

bitToByte :: AsmAddressType -> ByteType
bitToByte = \case
    AsmZeroPage _ Nothing -> Literal 0x24
    AsmAbsolute _ Nothing -> Literal 0x2C
    _                     -> assertNotReached

adcToByte :: AsmAddressType -> ByteType
adcToByte = \case
    AsmImmediate _            -> Literal 0x69
    AsmZeroPage _ (Just AsmX) -> Literal 0x75
    AsmZeroPage _ Nothing     -> Literal 0x65
    AsmAbsolute _ (Just AsmX) -> Literal 0x7D
    AsmAbsolute _ (Just AsmY) -> Literal 0x79
    AsmAbsolute _ Nothing     -> Literal 0x6D
    AsmIndirect _ (Just AsmX) -> Literal 0x61
    AsmIndirect _ (Just AsmY) -> Literal 0x71
    _                         -> assertNotReached

sbcToByte :: AsmAddressType -> ByteType
sbcToByte = \case
    AsmImmediate _            -> Literal 0xE9
    AsmZeroPage _ (Just AsmX) -> Literal 0xF5
    AsmZeroPage _ Nothing     -> Literal 0xE5
    AsmAbsolute _ (Just AsmX) -> Literal 0xFD
    AsmAbsolute _ (Just AsmY) -> Literal 0xF9
    AsmAbsolute _ Nothing     -> Literal 0xED
    AsmIndirect _ (Just AsmX) -> Literal 0xE1
    AsmIndirect _ (Just AsmY) -> Literal 0xF1
    _                         -> assertNotReached

cmpToByte :: AsmAddressType -> ByteType
cmpToByte = \case
    AsmImmediate _            -> Literal 0xC9
    AsmZeroPage _ (Just AsmX) -> Literal 0xD5
    AsmZeroPage _ Nothing     -> Literal 0xC5
    AsmAbsolute _ (Just AsmX) -> Literal 0xDD
    AsmAbsolute _ (Just AsmY) -> Literal 0xD9
    AsmAbsolute _ Nothing     -> Literal 0xCD
    AsmIndirect _ (Just AsmX) -> Literal 0xC1
    AsmIndirect _ (Just AsmY) -> Literal 0xD1
    _                         -> assertNotReached

cpxToByte :: AsmAddressType -> ByteType
cpxToByte = \case
    AsmImmediate _        -> Literal 0xE0
    AsmZeroPage _ Nothing -> Literal 0xE4
    AsmAbsolute _ Nothing -> Literal 0xEC
    _                     -> assertNotReached

cpyToByte :: AsmAddressType -> ByteType
cpyToByte = \case
    AsmImmediate _        -> Literal 0xC0
    AsmZeroPage _ Nothing -> Literal 0xC4
    AsmAbsolute _ Nothing -> Literal 0xCC
    _                     -> assertNotReached

incToByte :: AsmAddressType -> ByteType
incToByte = \case
    AsmZeroPage _ (Just AsmX) -> Literal 0xF6
    AsmZeroPage _ Nothing     -> Literal 0xE6
    AsmAbsolute _ (Just AsmX) -> Literal 0xFE
    AsmAbsolute _ Nothing     -> Literal 0xEE
    _                         -> assertNotReached

decToByte :: AsmAddressType -> ByteType
decToByte = \case
    AsmZeroPage _ (Just AsmX) -> Literal 0xD6
    AsmZeroPage _ Nothing     -> Literal 0xC6
    AsmAbsolute _ (Just AsmX) -> Literal 0xDE
    AsmAbsolute _ Nothing     -> Literal 0xCE
    _                         -> assertNotReached

aslToByte :: AsmAddressType -> ByteType
aslToByte = \case
    AsmAccumulator            -> Literal 0x0A
    AsmZeroPage _ (Just AsmX) -> Literal 0x16
    AsmZeroPage _ Nothing     -> Literal 0x06
    AsmAbsolute _ (Just AsmX) -> Literal 0x1E
    AsmAbsolute _ Nothing     -> Literal 0x0E
    _                         -> assertNotReached

lsrToByte :: AsmAddressType -> ByteType
lsrToByte = \case
    AsmAccumulator            -> Literal 0x4A
    AsmZeroPage _ (Just AsmX) -> Literal 0x56
    AsmZeroPage _ Nothing     -> Literal 0x46
    AsmAbsolute _ (Just AsmX) -> Literal 0x5E
    AsmAbsolute _ Nothing     -> Literal 0x4E
    _                         -> assertNotReached

rolToByte :: AsmAddressType -> ByteType
rolToByte = \case
    AsmAccumulator            -> Literal 0x2A
    AsmZeroPage _ (Just AsmX) -> Literal 0x36
    AsmZeroPage _ Nothing     -> Literal 0x26
    AsmAbsolute _ (Just AsmX) -> Literal 0x3E
    AsmAbsolute _ Nothing     -> Literal 0x2E
    _                         -> assertNotReached

rorToByte :: AsmAddressType -> ByteType
rorToByte = \case
    AsmAccumulator            -> Literal 0x6A
    AsmZeroPage _ (Just AsmX) -> Literal 0x76
    AsmZeroPage _ Nothing     -> Literal 0x66
    AsmAbsolute _ (Just AsmX) -> Literal 0x7E
    AsmAbsolute _ Nothing     -> Literal 0x6E
    _                         -> assertNotReached

jmpToByte :: AsmAddressType -> ByteType
jmpToByte = \case
    AsmAbsolute _ Nothing -> Literal 0x4C
    AsmIndirect _ Nothing -> Literal 0x6C
    _                     -> assertNotReached
