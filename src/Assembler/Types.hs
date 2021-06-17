-- | Types for Parsing and Assembling
module Assembler.Types
    ( AsmStatement(..)
    , AsmRegisterName(..)
    , AsmDirectiveType(..)
    , AsmNumeric(..)
    , AsmAddressType(..)
    , AssemblyError(..)
    , renderStatements
    ) where

import           Data.Char                      ( intToDigit
                                                , toUpper
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Word                      ( Word16 )
import           Numeric                        ( showHex
                                                , showInt
                                                , showIntAtBase
                                                )
import           Text.Megaparsec                ( SourcePos )
import           Text.PrettyPrint               ( (<+>)
                                                , Doc
                                                , char
                                                , colon
                                                , comma
                                                , doubleQuotes
                                                , empty
                                                , equals
                                                , hcat
                                                , parens
                                                , punctuate
                                                , render
                                                , space
                                                , text
                                                )
import           Text.PrettyPrint.HughesPJClass ( Pretty(pPrint) )
import           Types                          ( OpName )

data AssemblyError = InvalidInstruction Text
                   | InvalidLabel Text
                   | InvalidVariable Text
                   | DuplicateLabel Text
                   | DuplicateVariable Text
                   deriving (Show)

data AsmStatement = StmtDirective AsmDirectiveType SourcePos
                  | StmtCodeLabel Text SourcePos
                  | StmtLabelLocation Text AsmDirectiveType SourcePos
                  | StmtDefineVar Text AsmNumeric SourcePos
                  | StmtProgramLocation AsmNumeric SourcePos
                  | StmtOpCode OpName AsmAddressType SourcePos
                  deriving (Show)

instance Eq AsmStatement where
    (StmtDirective dType1 _) == (StmtDirective dType2 _) = dType1 == dType2
    (StmtCodeLabel label1 _) == (StmtCodeLabel label2 _) = label1 == label2
    (StmtLabelLocation label1 dType1 _) == (StmtLabelLocation label2 dType2 _) =
        label1 == label2 && dType1 == dType2
    (StmtDefineVar var1 num1 _ ) == (StmtDefineVar var2 num2 _ ) = var1 == var2 && num1 == num2
    (StmtProgramLocation num1 _) == (StmtProgramLocation num2 _) = num1 == num2
    (StmtOpCode opn1 addrT1 _  ) == (StmtOpCode opn2 addrT2 _  ) = opn1 == opn2 && addrT1 == addrT2
    _                            == _                            = False


data AsmRegisterName = AsmA
                     | AsmX
                     | AsmY
                     deriving (Eq, Show)

data AsmDirectiveType = DtByte [AsmNumeric]
                      | DtDByte [AsmNumeric]
                      | DtWord [AsmNumeric]
                      | DtBlock AsmNumeric
                      | DtText Text
                      | DtEnd
                      deriving (Eq, Show)

data AsmNumeric = DecimalImmediate Word16
                | HexImmediate Word16
                | BinaryImmediate Word16
                | DecimalLiteral Word16
                | BinaryLiteral Word16
                | HexLiteral Word16
                deriving (Eq, Show)

-- NOTE: AsmImplicit is the result when StmtOpCode has a Nothing Value
-- We can't really parse anything if we don't know what OpCode we get
-- (not entirely true theres probably some functions that we can pass forward the OpCode)
-- Either way in the current implementation we don't actually use AsmImplicit
data AsmAddressType = AsmImmediate AsmNumeric
                    | AsmZeroPage AsmNumeric (Maybe AsmRegisterName)
                    | AsmAbsolute AsmNumeric (Maybe AsmRegisterName)
                    | AsmIndirect AsmNumeric (Maybe AsmRegisterName)
                    | AsmRelative Int
                    | AsmAccumulator
                    | AsmImplicit
                    | AsmUnknown Text
                    | AsmUnknownImm Text
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

signedDecNumToDoc :: Int -> Doc
signedDecNumToDoc val = sign <> text (showInt (abs val) "")
    where sign = if val < 0 then char '-' else char '+'

listAsmNumericToDoc :: [AsmNumeric] -> Doc
listAsmNumericToDoc = hcat . punctuate comma . map pPrint

newline, indent, immIdent, hexIdent, binIdent, programCounter :: Doc
newline = char '\n'
indent = char '\t'
immIdent = char '#'
hexIdent = char '$'
binIdent = char '%'
programCounter = char '*'

renderStatements :: [AsmStatement] -> Text
renderStatements = pack . render . hcat . map pPrint

instance Pretty AsmStatement where
    pPrint (StmtDirective dType _) = pPrint dType <> newline
    pPrint (StmtCodeLabel label _) = textToDoc label <> colon <> newline
    pPrint (StmtLabelLocation label dType _) =
        (textToDoc label <> colon) <+> (pPrint dType <> newline)
    pPrint (StmtDefineVar label val _) =
        text "define" <+> textToDoc label <+> pPrint val <+> newline
    pPrint (StmtProgramLocation val _) = (programCounter <> equals) <+> pPrint val <+> newline
    -- don't ask
    pPrint (StmtOpCode opname AsmImplicit _) = indent <> (opNameToDoc opname <+> newline)
    pPrint (StmtOpCode opname val _) = indent <> (opNameToDoc opname <+> pPrint val <+> newline)

instance Pretty AsmDirectiveType where
    pPrint (DtByte  val) = indent <> (text ".BYTE" <+> listAsmNumericToDoc val)
    pPrint (DtDByte val) = indent <> (text ".DBYTE" <+> listAsmNumericToDoc val)
    pPrint (DtWord  val) = indent <> (text ".WORD" <+> listAsmNumericToDoc val)
    pPrint (DtBlock val) = indent <> (text ".BLOCK" <+> pPrint val)
    pPrint (DtText  str) = indent <> (text ".TEXT" <+> doubleQuotes (textToDoc str))
    pPrint DtEnd         = text ".END"

instance Pretty AsmNumeric where
    pPrint (HexImmediate     val) = immIdent <> hexIdent <> hexNumToDoc val
    pPrint (BinaryImmediate  val) = immIdent <> binIdent <> binNumToDoc val
    pPrint (DecimalImmediate val) = immIdent <> decNumToDoc val
    pPrint (BinaryLiteral    val) = binIdent <> binNumToDoc val
    pPrint (DecimalLiteral   val) = decNumToDoc val
    pPrint (HexLiteral       val) = hexIdent <> hexNumToDoc val

instance Pretty AsmAddressType where
    pPrint (AsmImmediate val           ) = pPrint val
    pPrint (AsmZeroPage val Nothing    ) = pPrint val
    pPrint (AsmZeroPage val (Just reg) ) = pPrint val <> comma <> pPrint reg
    pPrint (AsmAbsolute val Nothing    ) = pPrint val
    pPrint (AsmAbsolute val (Just reg )) = pPrint val <> comma <> pPrint reg
    pPrint (AsmIndirect val (Just AsmX)) = parens (pPrint val <> comma <> space <> pPrint AsmX)
    pPrint (AsmIndirect val (Just AsmY)) = parens (pPrint val) <> comma <> space <> pPrint AsmY
    pPrint (AsmIndirect val _          ) = parens (pPrint val) -- FIXME: ignores blatant error on AsmA
    pPrint (AsmRelative val            ) = programCounter <> signedDecNumToDoc val
    pPrint AsmAccumulator                = pPrint AsmA
    pPrint AsmImplicit                   = empty
    pPrint (AsmUnknown    var)           = textToDoc var
    pPrint (AsmUnknownImm var)           = immIdent <> textToDoc var

instance Pretty AsmRegisterName where
    pPrint AsmA = text "A"
    pPrint AsmX = text "X"
    pPrint AsmY = text "Y"
