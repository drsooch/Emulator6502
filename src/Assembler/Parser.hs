-- |

module Assembler.Parser
    ( parseFile
    , parseAssembly
    ) where

import           Assembler.Types
import           Assembler.Utils
import           Data.Char                      ( isAlpha )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( (<?>)
                                                , Parsec
                                                , between
                                                , choice
                                                , empty
                                                , eof
                                                , getSourcePos
                                                , label
                                                , manyTill
                                                , optional
                                                , runParser
                                                , sepBy
                                                , takeWhile1P
                                                , try
                                                )
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Debug          ( dbg )
import           Text.Megaparsec.Error          ( ParseErrorBundle )
import           Types                          ( OpName(..) )

type AssemblerParser a = Parsec Void Text a
type ParserError = ParseErrorBundle Text Void

parseFile :: String -> IO (Either ParserError [AsmStatement])
parseFile filepath = TIO.readFile filepath <&> parseAssembly filepath

-- clear whitespace from the start
parseAssembly :: String -> Text -> Either ParserError [AsmStatement]
parseAssembly = runParser (whiteSpace *> manyTill asmStatement eof)

asmStatement :: AssemblerParser AsmStatement
asmStatement =
    dbg "AsmStatement" $ choice [pDirective, try pDefineVar, try pLabel, pProgramLocation, pOpCode]

whiteSpace :: AssemblerParser ()
whiteSpace = L.space C.space1 (L.skipLineComment ";") empty

whiteSpaceNoNewLine :: AssemblerParser ()
whiteSpaceNoNewLine = L.space C.hspace1 (L.skipLineComment ";") empty

lexeme :: AssemblerParser a -> AssemblerParser a
lexeme = L.lexeme whiteSpace

lexemeNoNewLine :: AssemblerParser a -> AssemblerParser a
lexemeNoNewLine = L.lexeme whiteSpaceNoNewLine

endOfLine :: AssemblerParser ()
endOfLine = whiteSpaceNoNewLine *> C.newline $> ()

symbol :: Text -> AssemblerParser Text
symbol = L.symbol whiteSpace

symbolNoNewLine :: Text -> AssemblerParser Text
symbolNoNewLine = L.symbol whiteSpaceNoNewLine

comma :: AssemblerParser ()
comma = symbol "," $> ()

colon :: AssemblerParser ()
colon = symbolNoNewLine ":" $> ()

equals :: AssemblerParser ()
equals = symbol "=" $> ()

parens :: AssemblerParser a -> AssemblerParser a
parens = between (symbol "(") (symbol ")")

directiveStart :: AssemblerParser ()
directiveStart = C.char '.' $> ()

progCountIdent :: AssemblerParser ()
progCountIdent = C.char '*' $> ()

hexIdent :: AssemblerParser ()
hexIdent = C.char '$' $> ()

binIdent :: AssemblerParser ()
binIdent = C.char '%' $> ()

immediateIdent :: AssemblerParser ()
immediateIdent = C.char '#' $> ()

stringConstant :: Text -> AssemblerParser Text
stringConstant = lexemeNoNewLine . C.string'

stringLiteral :: AssemblerParser Text
stringLiteral =
    dbg "String Literal"
        $   pack
        .   escapeChars
        <$> (C.char '"' *> manyTill L.charLiteral (C.char '"'))

variableString :: AssemblerParser Text
variableString =
    dbg "Variable String" $ takeWhile1P (Just "Define Variable") (\c -> isAlpha c || c == '_')

{---------------------- NUMERICS -----------------------}
offset :: AssemblerParser Int
offset = L.signed whiteSpaceNoNewLine L.decimal

pHexImmediate :: AssemblerParser AsmNumeric
pHexImmediate =
    dbg "Hex Immediate" $ HexImmediate <$> (immediateIdent >> hexIdent >> L.hexadecimal)

pBinaryImmediate :: AssemblerParser AsmNumeric
pBinaryImmediate =
    dbg "Binary Immediate" $ BinaryImmediate <$> (immediateIdent >> binIdent >> L.binary)

pDecimalImmediate :: AssemblerParser AsmNumeric
pDecimalImmediate = dbg "Decimal Immediate" $ DecimalImmediate <$> (immediateIdent >> L.decimal)

pBinaryLiteral :: AssemblerParser AsmNumeric
pBinaryLiteral = dbg "BinaryLiteral" $ BinaryLiteral <$> (binIdent >> L.binary)

pDecimalLiteral :: AssemblerParser AsmNumeric
pDecimalLiteral = dbg "Decimal Literal" $ DecimalLiteral <$> L.decimal

pHexLiteral :: AssemblerParser AsmNumeric
pHexLiteral = dbg "Hex Literal" $ HexLiteral <$> (hexIdent >> L.hexadecimal)

pNumericImmediate :: AssemblerParser AsmNumeric
pNumericImmediate = dbg
    "pNumeric Immediate"
    (choice [try pHexImmediate, try pBinaryImmediate, pDecimalImmediate] <?> "Numeric Immediate")

pNumericLiteral :: AssemblerParser AsmNumeric
pNumericLiteral = dbg
    "Numeric Literal"
    (choice [pHexLiteral, pBinaryLiteral, pDecimalLiteral] <?> "Numeric Literal")

{---------------------- Directives -----------------------}
pDTByte :: AssemblerParser AsmDirectiveType
pDTByte =
    dbg "DTBYTE" (DtByte <$ stringConstant "BYTE" <*> pNumericLiteral `sepBy` comma <* endOfLine)

pDTDByte :: AssemblerParser AsmDirectiveType
pDTDByte =
    dbg "DTDBYTE" (DtDByte <$ stringConstant "DBYTE" <*> pNumericLiteral `sepBy` comma <* endOfLine)

pDTWord :: AssemblerParser AsmDirectiveType
pDTWord =
    dbg "DTWORD" (DtWord <$ stringConstant "WORD" <*> pNumericLiteral `sepBy` comma <* endOfLine)

pDTBlock :: AssemblerParser AsmDirectiveType
pDTBlock = dbg "DTBLOCK" (DtBlock <$ stringConstant "BLOCK" <*> pDecimalLiteral <* endOfLine)

pDTText :: AssemblerParser AsmDirectiveType
pDTText = dbg "DTTEXT" (DtText <$ stringConstant "TEXT" <*> stringLiteral <* endOfLine)

-- we don't want to chew up all whiteSpace using String Constant
-- we can just use endOfLine instead
pDTEnd :: AssemblerParser AsmDirectiveType
pDTEnd = dbg "DTEND" (DtEnd <$ C.string' "END" <* endOfLine)

pDirective' :: AssemblerParser AsmDirectiveType
pDirective' = dbg
    "Directives"
    (   directiveStart
    >>  choice [pDTByte, pDTDByte, pDTWord, pDTBlock, pDTText, pDTEnd]
    <?> "Directive Type"
    )

pDirective :: AssemblerParser AsmStatement
pDirective = dbg "Directive w/ SourcePos" $ lexeme $ StmtDirective <$> pDirective' <*> getSourcePos

{---------------------- Labels -----------------------}
pLabel' :: AssemblerParser Text
pLabel' = dbg "Label Name" $ takeWhile1P (Just "Label Name") isAlpha <* colon

pLabeledLocation :: AssemblerParser AsmStatement
pLabeledLocation =
    dbg "Labeled Location"
        $   label "Labeled Location"
        $   StmtLabelLocation
        <$> pLabel'
        <*> pDirective'
        <*> getSourcePos

pCodeLabel :: AssemblerParser AsmStatement
pCodeLabel =
    dbg "Code Label" $ label "Code Label" $ StmtCodeLabel <$> pLabel' <* endOfLine <*> getSourcePos

pLabel :: AssemblerParser AsmStatement
pLabel = lexeme $ choice [try pLabeledLocation, pCodeLabel]

{---------------------- Defines -----------------------}
pDefineVar :: AssemblerParser AsmStatement
pDefineVar =
    dbg "Define Variable"
        $   lexeme
        $   StmtDefineVar
        <$> (stringConstant "define" >> variableString <* whiteSpace)
        <*> pNumericLiteral
        <*> getSourcePos

{---------------------- Addresses -----------------------}
pARegister :: AssemblerParser AsmRegisterName
pARegister = dbg "Asm A" $ C.char' 'A' $> AsmA

pXRegister :: AssemblerParser AsmRegisterName
pXRegister = dbg "Asm X" $ C.char' 'X' $> AsmX

pYRegister :: AssemblerParser AsmRegisterName
pYRegister = dbg "Asm Y" $ C.char' 'Y' $> AsmY

pAddressRegister :: AssemblerParser AsmRegisterName
pAddressRegister = dbg "Address Register" (choice [pXRegister, pYRegister] <?> "X or Y")

pAccumAddress :: AssemblerParser AsmAddressType
pAccumAddress = dbg "Accumulator Address" $ pARegister *> endOfLine $> AsmAccumulator

pImmediateAddress :: AssemblerParser AsmAddressType
pImmediateAddress = dbg "Immediate Address" $ AsmImmediate <$> pNumericImmediate <* endOfLine

pZeroPageOrAbsolute :: AssemblerParser AsmAddressType
pZeroPageOrAbsolute = dbg "Zero Page Or Absolute" $ label "ZeroPage or Absolute Address" $ do
    hl@(HexLiteral val) <- pHexLiteral
    addrReg             <- optional (comma >> pAddressRegister)
    _                   <- endOfLine
    if val > 0xFF then pure $ AsmAbsolute hl addrReg else pure $ AsmZeroPage hl addrReg

pIndirect :: AssemblerParser AsmAddressType
pIndirect = dbg "Indirect Address" $ flip AsmIndirect Nothing <$> parens pHexLiteral

pIndirectX :: AssemblerParser AsmAddressType
pIndirectX = dbg "Indirect X Address"
    $ parens (pHexLiteral >>= \num -> comma >> AsmIndirect num . Just <$> pXRegister)

pIndirectY :: AssemblerParser AsmAddressType
pIndirectY =
    dbg "Indirect Y Address"
        $   AsmIndirect
        <$> parens pHexLiteral
        <*> (comma >> Just <$> pYRegister)
        <*  endOfLine

pIndirectAddress :: AssemblerParser AsmAddressType
pIndirectAddress = dbg "Indirect Address" $ label "Indirect Address" $ choice
    [try pIndirectX, try pIndirectY, pIndirect]

pRelative :: AssemblerParser AsmAddressType
pRelative =
    dbg "Relative address"
        $   label "Relative Address"
        $   AsmRelative
        <$> (progCountIdent >> offset)
        <*  endOfLine

pAddressLabel :: AssemblerParser AsmAddressType
pAddressLabel = dbg "Address Label" $ label "Address w/ Label or Variable" $ choice
    [immediateIdent *> (AsmUnknownImm <$> variableString), AsmUnknown <$> variableString]


pAddressType :: AssemblerParser AsmAddressType
pAddressType = dbg "Address Type" $ choice
    [ C.newline $> AsmImplicit
    , pZeroPageOrAbsolute
    , pIndirectAddress
    , pImmediateAddress
    , pRelative
    , try pAccumAddress
    , pAddressLabel
    ]

{---------------------- OpCodes -----------------------}
pOpCode :: AssemblerParser AsmStatement
pOpCode = dbg "pOpCode" $ lexeme $ StmtOpCode <$> pOpCode' <*> pAddressType <*> getSourcePos

pOpCode' :: AssemblerParser OpName
pOpCode' = dbg "OpCode" $ choice $ map
    (\opn -> lexemeNoNewLine (opn <$ C.string' (pack $ show opn)))
    [minBound .. maxBound]

{---------------------- Program Location -----------------------}
pProgramLocation :: AssemblerParser AsmStatement
pProgramLocation =
    dbg "Program Location"
        $   label "Program Location"
        $   lexeme
        $   StmtProgramLocation
        <$> (progCountIdent >> equals >> pHexLiteral)
        <*> getSourcePos
