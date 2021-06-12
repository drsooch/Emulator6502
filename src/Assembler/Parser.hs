-- | Module for parsing 6502 Assembly
-- module Assembler.Parser
--     ( pDirective
--     , pEndDirective
--     , parseFile
--     , AsmStatement(..)
--     , AsmNumeric(..)
--     , AsmDirectiveType(..)
--     , asmStatement
--     ) where

module Assembler.Parser
    ( module Assembler.Parser
    ) where

import           Assembler.Types
import           Data.Char                      ( isAlpha )
import           Data.Functor                   ( ($>) )
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
                                                , manyTill
                                                , optional
                                                , runParser
                                                , takeWhileP
                                                , try
                                                )
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.PrettyPrint               ( hcat
                                                , render
                                                )
import           Text.PrettyPrint.HughesPJClass ( pPrint )
import           Types                          ( OpName(..) )

type AssemblerParser a = Parsec Void Text a


parseFile :: String -> IO ()
--parseFile filepath = TIO.readFile filepath >>= runParser (manyTill asmStatement eof) filepath
parseFile filepath = do
    contents <- TIO.readFile filepath
    case runParser (manyTill asmStatement eof) filepath contents of
        Left  err    -> print err
        Right result -> print result >> (putStrLn $ render $ hcat $ map pPrint result)


asmStatement :: AssemblerParser AsmStatement
asmStatement = choice [try pOpCode, pDirective, try pDefineLabel, pLabel]

whiteSpace :: AssemblerParser ()
whiteSpace = L.space C.space1 (L.skipLineComment ";") empty

lexeme :: AssemblerParser a -> AssemblerParser a
lexeme = L.lexeme whiteSpace

symbol :: Text -> AssemblerParser Text
symbol = L.symbol whiteSpace

comma :: AssemblerParser ()
comma = symbol "," $> ()

colon :: AssemblerParser ()
colon = symbol ":" $> ()

directiveStart :: AssemblerParser ()
directiveStart = C.char '.' $> ()

stringConstant :: Text -> AssemblerParser Text
stringConstant = lexeme . C.string'

equals :: AssemblerParser ()
equals = symbol "=" $> ()

parens :: AssemblerParser a -> AssemblerParser a
parens = between (symbol "(") (symbol ")")

hexIdent :: AssemblerParser ()
hexIdent = C.char '$' $> ()

binIdent :: AssemblerParser ()
binIdent = C.char '%' $> ()

literalIdent :: AssemblerParser ()
literalIdent = C.char '#' $> ()

stringLiteral :: AssemblerParser Text
stringLiteral = pack <$> lexeme (C.char '\"' *> manyTill L.charLiteral (C.char '\"'))

{- NUMERICS -}
pHexLiteral :: AssemblerParser AsmNumeric
pHexLiteral = HexLiteral <$> (literalIdent >> hexIdent >> L.hexadecimal)

pBinaryLiteral :: AssemblerParser AsmNumeric
pBinaryLiteral = BinaryLiteral <$> (literalIdent >> binIdent >> L.binary)

pDecimalLiteral :: AssemblerParser AsmNumeric
pDecimalLiteral = DecimalLiteral <$> (literalIdent >> L.decimal)

pHexAddress :: AssemblerParser AsmNumeric
pHexAddress = HexAddress <$> (hexIdent >> L.hexadecimal)

pNumeric :: AssemblerParser AsmNumeric
pNumeric = lexeme
    (choice [try pHexLiteral, try pBinaryLiteral, try pDecimalLiteral, pHexAddress] <?> "Numeric")

pDirective :: AssemblerParser AsmStatement
pDirective =
    StmtDirective
        <$> (   directiveStart
            >>  choice
                    [ DtByte <$ stringConstant "BYTE" <*> pNumeric
                    , DtDByte <$ stringConstant "DBYTE" <*> pNumeric
                    , DtWord <$ stringConstant "WORD" <*> pNumeric
                    , DtBlock <$ stringConstant "BLOCK" <*> pNumeric
                    , DtEqu <$ stringConstant "EQU" <*> pNumeric
                    , DtText <$ stringConstant "TEXT" <*> stringLiteral
                    , DtEnd <$ stringConstant "END"
                    ]
            <?> "Directive"
            )

-- TODO: Implement 6 Character Limit on Labels as defined by the resource book.
pLabel :: AssemblerParser AsmStatement
pLabel = StmtCodeLabel <$> lexeme (takeWhileP (Just "Code Label") isAlpha <* optional colon)

-- TODO: See `pLabel`
pDefineLabel :: AssemblerParser AsmStatement
pDefineLabel =
    StmtDefineLabel
        <$> takeWhileP (Just "Define Label") isAlpha
        <*  whiteSpace
        <*  equals
        <*> pNumeric

pARegister :: AssemblerParser AsmRegisterName
pARegister = AsmA <$ lexeme (C.string' "A")

pXRegister :: AssemblerParser AsmRegisterName
pXRegister = AsmX <$ lexeme (C.string' "X")

pYRegister :: AssemblerParser AsmRegisterName
pYRegister = AsmY <$ lexeme (C.string' "Y")

-- Parse any register name
pRegister :: AssemblerParser AsmRegisterName
pRegister = choice [pARegister, pXRegister, pYRegister] <?> "Register"

-- parse just X and Y for use with an instruction address
pAddressRegister :: AssemblerParser AsmRegisterName
pAddressRegister = choice [pXRegister, pYRegister] <?> "Address Register"

pAddress :: AssemblerParser AsmAddressType
pAddress =
    choice
            [ pAccumAddr
            , pZeroPageOrAbsolute
            , pIndirectAddr
            , pImmediateAddr
            --, pRelative
            ]
        <?> "AddressType"

pAccumAddr :: AssemblerParser AsmAddressType
pAccumAddr = AsmAccumulator <$ pARegister

pImmediateAddr :: AssemblerParser AsmAddressType
pImmediateAddr = AsmImmediate <$> pNumeric

pZeroPageOrAbsolute :: AssemblerParser AsmAddressType
pZeroPageOrAbsolute = do
    ha@(HexAddress hexAddr) <- pHexAddress
    addrReg                 <- optional (comma >> pAddressRegister)
    if hexAddr > 0xFF
        then pure $ AsmAbsolute ha addrReg
        else pure $ AsmZeroPage (HexByte $ fromIntegral hexAddr) addrReg

pIndirectAddr :: AssemblerParser AsmAddressType
pIndirectAddr = choice [try pIndirectX, try pIndirectY, pIndirect]

-- should be a memory address with no register
pIndirect :: AssemblerParser AsmAddressType
pIndirect = flip AsmIndirect Nothing <$> parens pNumeric

pIndirectX :: AssemblerParser AsmAddressType
pIndirectX = parens (pNumeric >>= \num -> comma >> AsmIndirect num . Just <$> pAddressRegister)

pIndirectY :: AssemblerParser AsmAddressType
pIndirectY = AsmIndirect <$> parens pNumeric <*> (comma >> Just <$> pAddressRegister)

-- FIXME
pRelative :: AssemblerParser AsmAddressType
pRelative = pure AsmRelative

pOpCode :: AssemblerParser AsmStatement
pOpCode = StmtOpCode <$> pOpCode' <*> optional pAddress

pOpCode' :: AssemblerParser OpName
pOpCode' = choice [ADC <$ stringConstant "ADC"]
