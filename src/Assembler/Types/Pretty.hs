-- | PrettyPrinting Utilities

module Assembler.Types.Pretty
    ( textToDoc
    , opNameToDoc
    , hexNumToDoc
    , binNumToDoc
    , decNumToDoc
    , signedDecNumToDoc
    , listToDoc
    , newline
    , indent
    , immIdent
    , hexIdent
    , binIdent
    , programCounter
    , renderStatements
    , module Text.PrettyPrint
    , Pretty(pPrint)
    ) where

import           Data.Char                      ( intToDigit
                                                , toUpper
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
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
                                                , empty
                                                , hcat
                                                , parens
                                                , punctuate
                                                , render
                                                , space
                                                , text
                                                )
import           Text.PrettyPrint.HughesPJClass ( Pretty(pPrint) )
import           Types                          ( OpName )

textToDoc :: Text -> Doc
textToDoc = text . T.unpack

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

listToDoc :: Pretty a => [a] -> Doc
listToDoc = hcat . punctuate comma . map pPrint

newline, indent, immIdent, hexIdent, binIdent, programCounter :: Doc
newline = char '\n'
indent = char '\t'
immIdent = char '#'
hexIdent = char '$'
binIdent = char '%'
programCounter = char '*'

renderStatements :: Pretty a => [a] -> Text
renderStatements = T.pack . render . hcat . map pPrint
