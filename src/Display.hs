-- | Display Contents of CPUState and Memory
module Display
    ( showNBytes
    , Display(..)
    , withTab
    , withNewLine
    , indentLine
    ) where

import qualified Data.Array.IArray             as IA
import           Data.Word
import           Numeric                        ( showHex )
import           Types


showNBytes :: Address -> Offset -> CPUState -> [String]
showNBytes addr offset CPU {..} =
    [ showHex i "" <> ": " <> showHex (memory IA.! i) ""
    | i <- [addr .. (addr + offset)]
    ]

withTab :: String -> String
withTab str = "\t" <> str

withNewLine :: String -> String
withNewLine str = str <> "\n"

indentLine :: String -> String
indentLine = withTab . withNewLine

-- | Display is simply a Show instance for the User.
class Display a where
  display :: a -> String

instance Display Word8 where
    display val = "0x" <> showHex val ""

instance Display Word16 where
    display val = "0x" <> showHex val ""

instance Display Register where
    display (Reg val) = "Reg: " <> display val

instance Display Flags where
    display (Flags val) = "Flags: " <> display val

instance Display StackPointer where
    display (SP val) = "StackPointer: " <> display val

instance Display ProgramCounter where
    display (PC val) = "ProgramCounter: " <> display val

instance Display CPUState where
    display cpu =
        "Current CPUState:\n"
            <> indentLine (display (aReg cpu))
            <> indentLine (display (xReg cpu))
            <> indentLine (display (yReg cpu))
            <> indentLine (display (fReg cpu))
            <> indentLine (display (sp cpu))
            <> indentLine (display (pc cpu))



instance Display Instruction where
    display (Instruction opc op) =
        "Instruction:\n" <> indentLine (display opc) <> indentLine (display op)

instance Display OpCode where
    display (OpCode opn addrt) = withNewLine ("OpCode - " <> show opn)
        <> withTab ("Addressing Type - " <> show addrt)

instance Display Operand where
    display (Operand opt storeloc) =
        withNewLine ("OperandType - " <> display opt)
            <> withTab ("StoreLocation - " <> display storeloc)

instance Display OperandType where
    display = \case
        (OpTMemory addr) -> "From Memory Loc: " <> display addr
        (OpTValue  byte) -> "From Value: " <> display byte
        OpTEmpty         -> "No Operand"

instance Display StoreLoc where
    display = \case
        (MemorySL   addr)       -> "Store to Address: " <> display addr
        (RegisterSL AReg)       -> "Store to A Register"
        (RegisterSL XReg)       -> "Store to X Register"
        (RegisterSL YReg)       -> "Store to Y Register"
        (RegisterSL FReg)       -> "Store to Status Flags"
        -- FIXME
        StackPointerSL          -> "Store to Stack Pointer"
        (ProgramCounterSL addr) -> "Jumping to: " <> display addr
        NoStore                 -> "No Store"

instance Show a => Display [a] where
    display s = show s
