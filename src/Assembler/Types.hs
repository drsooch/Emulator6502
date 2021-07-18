-- | Types for Parsing and Assembling
module Assembler.Types
    ( AsmStatement(..)
    , ProgramLocation(..)
    , CodeBlock(..)
    , CodeStatement(..)
    , LabeledLocation(..)
    , VarDefinition(..)
    , Bindings(..)
    , AsmAddressType(..)
    , AsmDirectiveType(..)
    , AsmRegisterName(..)
    , AsmNumeric(..)
    , AsmState(..)
    , AssemblyExe(..)
    , AsmTree(..)
    , ByteType(..)
    , Assembler
    , Variables
    , CodeLabels
    , MemLocations
    , renderStatements
    ) where

import           Control.Monad.State.Strict     ( State )
import           Data.Char                      ( intToDigit
                                                , toUpper
                                                )
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Word                      ( Word16 )
import           GHC.Generics                   ( Generic )
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
                                                , hcat
                                                , parens
                                                , punctuate
                                                , render
                                                , space
                                                , text
                                                )
import           Text.PrettyPrint.HughesPJClass ( Pretty(pPrint) )
import           Types

-- "AST" for file
data AsmTree = AsmTree
    { progStartIdent :: Maybe ProgramLocation
    , definitions    :: [VarDefinition]
    , codeBlocks     :: [CodeBlock]
    , labeledLocs    :: [LabeledLocation]
    }
    deriving (Show, Generic)

data AsmStatement = StmtCodeBlock CodeBlock
                  | StmtLabeledLocation LabeledLocation
                  | StmtDefineVar VarDefinition
                  | StmtProgramLocation ProgramLocation
                  deriving (Show, Eq)

data ProgramLocation = ProgramLoc
    { location  :: AsmNumeric
    , sourcePos :: SourcePos
    }
    deriving Show

instance Eq ProgramLocation where
    ProgramLoc l1 _ == ProgramLoc l2 _ = l1 == l2

data CodeBlock = CodeBlock
    { progOffset :: Int
    , label      :: Text
    , statements :: [CodeStatement]
    }
    deriving Show

instance Eq CodeBlock where
    CodeBlock _ l1 b1 == CodeBlock _ l2 b2 = l1 == l2 && b1 == b2

data CodeStatement = CodeStatement
    { opName      :: OpName
    , addressType :: AsmAddressType
    , sourcePos   :: SourcePos
    }

instance Eq CodeStatement where
    CodeStatement opn1 addr1 _ == CodeStatement opn2 addr2 _ = opn1 == opn2 && addr1 == addr2

instance Show CodeStatement where
    show (CodeStatement opn addrT _) = "CodeStatement " <> show opn <> " " <> show addrT

data LabeledLocation = LabeledLoc
    { label      :: Text
    , directType :: AsmDirectiveType
    , sourcePos  :: SourcePos
    }
    deriving Show

instance Eq LabeledLocation where
    LabeledLoc l1 d1 _ == LabeledLoc l2 d2 _ = l1 == l2 && d1 == d2

data VarDefinition = VarDefinition
    { label     :: Text
    , value     :: AsmNumeric
    , sourcePos :: SourcePos
    }
    deriving Show

instance Eq VarDefinition where
    VarDefinition l1 v1 _ == VarDefinition l2 v2 _ = l1 == l2 && v1 == v2

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

renderStatements :: Pretty a => [a] -> Text
renderStatements = pack . render . hcat . map pPrint

instance Pretty AsmStatement where
    pPrint (StmtCodeBlock       codeBlock ) = pPrint codeBlock
    pPrint (StmtLabeledLocation labeledLoc) = pPrint labeledLoc
    pPrint (StmtDefineVar       defineVar ) = pPrint defineVar
    pPrint (StmtProgramLocation progLoc   ) = pPrint progLoc

instance Pretty CodeBlock where
    pPrint CodeBlock {..} = textToDoc label <> colon <> newline <> hcat (map pPrint statements)

instance Pretty CodeStatement where
    pPrint CodeStatement {..} = indent <> (opNameToDoc opName <+> pPrint addressType) <> newline

instance Pretty LabeledLocation where
    pPrint LabeledLoc {..} = ((textToDoc label <> colon) <+> pPrint directType) <> newline

instance Pretty VarDefinition where
    pPrint VarDefinition {..} = text "define" <+> textToDoc label <+> pPrint value <+> newline

instance Pretty ProgramLocation where
    pPrint ProgramLoc {..} = text "*=" <+> pPrint location <+> newline

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


{----------------------- Analysis and CodeGen ------------------------}
-- Map Defined Vars to their Value
type Variables = Map Text AsmNumeric
-- Map Labeled Memory Locations to a size in the data section
-- (Memory Offset, Size of Memory)
type MemLocations = Map Text (Int, Int)
-- Map Code Label to an Address (unintialized to start)
type CodeLabels = Map Text Address

type Assembler a = State AsmState a

data AsmState = AsmState
    { codeOffset :: Int
    , dataOffset :: Int
    , asmTree    :: AsmTree
    , bindings   :: Bindings
    }
    deriving Generic

data Bindings = Bindings
    { variables   :: Variables
    , codeLabels  :: CodeLabels
    , labeledLocs :: MemLocations
    }
    deriving (Show, Generic)

data AssemblyExe = Executable
    { dataBytes :: [ByteType]    -- "Data" section
    , codeBytes :: [ByteType]    -- "Code" section
    , progStart :: Maybe Address  -- Where to load the code
    }
    deriving Show


data ByteType = Literal Byte
              | Label Text
              | Offset Int [Byte]
              deriving (Show)
