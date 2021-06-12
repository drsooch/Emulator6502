-- | Display Contents of CPUState and Memory
{-# LANGUAGE FlexibleInstances #-}
module Display
    ( showNBytes
    , Display(..)
    , withTab
    , withNewLine
    , indentLine
    ) where

import qualified Data.Array.IArray             as IA
import           Data.Char                      ( toUpper )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Word
import qualified Numeric
import           Types


showHex :: (Integral a, Show a) => a -> Text
showHex = (<>) "0x" . pack . map toUpper . flip Numeric.showHex ""

showNBytes :: Address -> Offset -> CPUState -> [Text]
showNBytes addr offset CPU {..} =
    [ showHex i <> ": " <> showHex (memory IA.! i) | i <- [addr .. (addr + offset)] ]

withTab :: Text -> Text
withTab str = "\t" <> str

withNewLine :: Text -> Text
withNewLine str = str <> "\n"

indentLine :: Text -> Text
indentLine = withTab . withNewLine

-- | Display is simply a Show instance for the User.
class Display a where
  display :: a -> Text

instance Display Word8 where
    display = showHex

instance Display Word16 where
    display = showHex

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
            <> indentLine ("A" <> display (aReg cpu))
            <> indentLine ("X" <> display (xReg cpu))
            <> indentLine ("Y" <> display (yReg cpu))
            <> indentLine (display (fReg cpu))
            <> indentLine (display (sp cpu))
            <> indentLine (display (pc cpu))

instance Display Instruction where
    display (Instruction opc op) =
        "Instruction:\n" <> indentLine (display opc) <> indentLine (display op)

instance Display OpCode where
    display (OpCode opn addrt) =
        withNewLine "OpCode - " <> pack (show opn) <> withTab "Addressing Type - " <> pack
            (show addrt)

instance Display Operand where
    display (Operand opt storeloc) = withNewLine ("OperandType - " <> display opt)
        <> withTab ("StoreLocation - " <> display storeloc)

instance Display OperandType where
    display = \case
        (OpTMemory addr) -> "From Memory Loc: " <> display addr
        (OpTValue  byte) -> "From Value: " <> display byte
        OpTAccumulator   -> "Accumulator"
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

-- Use of Flexible Instances here
instance Display Text where
    display s = s
