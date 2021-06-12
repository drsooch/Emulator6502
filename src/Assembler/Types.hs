-- | Types for Parsing and Assembling
module Assembler.Types
    ( AsmStatement(..)
    , AsmRegisterName(..)
    , AsmDirectiveType(..)
    , AsmNumeric(..)
    , AsmAddressType(..)
    ) where

import           Data.Char                      ( intToDigit
                                                , toUpper
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Word                      ( Word16
                                                , Word8
                                                )
import           Numeric                        ( showHex
                                                , showInt
                                                , showIntAtBase
                                                )
import           Text.PrettyPrint               ( (<+>)
                                                , Doc
                                                , char
                                                , colon
                                                , comma
                                                , doubleQuotes
                                                , equals
                                                , parens
                                                , space
                                                , text
                                                )
import           Text.PrettyPrint.HughesPJClass ( Pretty(pPrint) )
import           Types                          ( OpName )

data AsmStatement = StmtDirective AsmDirectiveType
                  | StmtCodeLabel Text
                  | StmtDefineLabel Text AsmNumeric
                  | StmtProgramCounter (Maybe Int)
                  | StmtOpCode OpName (Maybe AsmAddressType)
                  deriving (Show, Eq)

data AsmRegisterName = AsmA
                     | AsmX
                     | AsmY
                     deriving (Eq, Show)

data AsmDirectiveType = DtByte AsmNumeric
                      | DtDByte AsmNumeric
                      | DtWord AsmNumeric
                      | DtBlock AsmNumeric
                      | DtEqu AsmNumeric
                      | DtText Text
                      | DtEnd
                      deriving (Eq, Show)

data AsmNumeric = DecimalLiteral Word16
                | HexLiteral Word16
                | BinaryLiteral Word16
                | HexAddress Word16
                | HexByte Word8
                deriving (Eq, Show)

-- NOTE: AsmImplicit is the result when StmtOpCode has a Nothing Value
-- We can't really parse anything if we don't know what OpCode we get
-- (not entirely true theres probably some functions that we can pass forward the OpCode)
-- Either way in the current implementation we don't actually use AsmImplicit
data AsmAddressType = AsmImmediate AsmNumeric
                    | AsmZeroPage AsmNumeric (Maybe AsmRegisterName)
                    | AsmAbsolute AsmNumeric (Maybe AsmRegisterName)
                    | AsmIndirect AsmNumeric (Maybe AsmRegisterName)
                    | AsmRelative
                    | AsmAccumulator
                    -- AsmImplicit
                    deriving (Eq, Show)


textToDoc :: Text -> Doc
textToDoc = text . unpack

opNameToDoc :: OpName -> Doc
opNameToDoc = text . show

hexNumToDoc :: (Integral a, Show a) => a -> Doc
hexNumToDoc = text . map toUpper . flip showHex ""

binNumToDoc :: (Integral a, Show a) => a -> Doc
binNumToDoc val = text $ showIntAtBase 2 intToDigit val ""

decNumToDoc :: (Integral a, Show a) => a -> Doc
decNumToDoc = text . flip showInt ""

newline, litIdent, hexIdent, binIdent :: Doc
newline = char '\n'
litIdent = char '#'
hexIdent = char '$'
binIdent = char '%'

instance Pretty AsmStatement where
    pPrint (StmtDirective dType      ) = pPrint dType <> newline
    pPrint (StmtCodeLabel label      ) = textToDoc label <> colon <> newline
    pPrint (StmtDefineLabel label val) = textToDoc label <+> equals <+> pPrint val <+> newline
    pPrint (StmtProgramCounter _     ) = undefined
    pPrint (StmtOpCode opname rhs    ) = opNameToDoc opname <+> pPrint rhs <+> newline

instance Pretty AsmDirectiveType where
    pPrint (DtByte  val) = text ".BYTE" <+> pPrint val
    pPrint (DtDByte val) = text ".DBYTE" <+> pPrint val
    pPrint (DtWord  val) = text ".WORD" <+> pPrint val
    pPrint (DtBlock val) = text ".BLOCK" <+> pPrint val
    pPrint (DtEqu   val) = text ".EQU" <+> pPrint val
    pPrint (DtText  str) = text ".TEXT" <+> doubleQuotes (textToDoc str)
    pPrint DtEnd         = text ".END"

instance Pretty AsmNumeric where
    pPrint (HexLiteral     val) = litIdent <> hexIdent <> hexNumToDoc val
    pPrint (BinaryLiteral  val) = litIdent <> binIdent <> binNumToDoc val
    pPrint (DecimalLiteral val) = litIdent <> decNumToDoc val
    pPrint (HexAddress     val) = hexIdent <> hexNumToDoc val
    pPrint (HexByte        val) = hexIdent <> hexNumToDoc val

instance Pretty AsmAddressType where
    pPrint (AsmImmediate val           ) = pPrint val
    pPrint (AsmZeroPage val Nothing    ) = pPrint val
    pPrint (AsmZeroPage val (Just reg) ) = pPrint val <> comma <> pPrint reg
    pPrint (AsmAbsolute val Nothing    ) = pPrint val
    pPrint (AsmAbsolute val (Just reg )) = pPrint val <> comma <> pPrint reg
    pPrint (AsmIndirect val (Just AsmX)) = parens (pPrint val <> comma <> space <> pPrint AsmX)
    pPrint (AsmIndirect val (Just AsmY)) = parens (pPrint val) <> comma <> space <> pPrint AsmY
    pPrint (AsmIndirect val _          ) = parens (pPrint val)
    pPrint AsmRelative                   = undefined
    pPrint AsmAccumulator                = pPrint AsmA

instance Pretty AsmRegisterName where
    pPrint AsmA = text "A"
    pPrint AsmX = text "X"
    pPrint AsmY = text "Y"
