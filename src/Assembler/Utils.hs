-- | Utility Functions for the assembler

module Assembler.Utils
    ( escapeChars
    , getAsmNumericVal
    ) where

import           Assembler.Types
import           Data.Word                      ( Word16 )

escapeChars :: String -> String
escapeChars = init . tail . show

getAsmNumericVal :: AsmNumeric -> Word16
getAsmNumericVal = \case
    HexLiteral       n -> n
    HexImmediate     n -> n
    DecimalImmediate n -> n
    DecimalLiteral   n -> n
    BinaryImmediate  n -> n
    BinaryLiteral    n -> n
