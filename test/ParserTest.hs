-- | Parsing Test

{-# OPTIONS_GHC -Wno-orphans #-}
module ParserTest
    ( parsing
    ) where

import           Assembler.Parser               ( parseAssembly )
import           Assembler.Types
import           Assembler.Utils
import           Control.Monad                  ( replicateM )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Test.QuickCheck
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.QuickCheck
import           Text.Megaparsec                ( mkPos )
import           Text.Megaparsec.Pos            ( SourcePos(..) )
import           Types                          ( OpName )

parsing :: TestTree
parsing = testGroup "Parsing" [parsingTest]

parsingTest :: TestTree
parsingTest = testProperty "Parser" prop_parse

prop_parse :: [AsmStatement] -> Property
prop_parse stmts = case parseAssembly "QuickCheck Parser" (renderStatements stmts) of
    Left  _      -> property False
    Right result -> stmts === result

genVarName :: Gen Text
genVarName = sized $ \n -> T.pack <$> replicateM n validSymbols
  where
    validSymbols = frequency [(26, choose ('a', 'z')), (26, choose ('A', 'Z')), (1, pure '_')]

genLabel :: Gen Text
genLabel = sized $ \n -> T.pack <$> replicateM n validSymbols
    where validSymbols = frequency [(26, choose ('a', 'z')), (26, choose ('A', 'Z'))]

genOpName :: Gen OpName
genOpName = chooseEnum (minBound, maxBound)

genRandomText :: Gen Text
genRandomText = sized $ \n -> T.pack . escapeChars <$> replicateM n arbitraryASCIIChar

genNumLiteral :: Gen AsmNumeric
genNumLiteral = oneof [genHexLiteral, genDecimalLiteral, genBinaryLiteral]

genHexLiteral :: Gen AsmNumeric
genHexLiteral = HexLiteral <$> arbitrary

-- need this and genHexByte to please QuickCheck, our PrettyPrinter
-- outputs the same text for AsmZeroPage and AsmAbsolute
-- despite having two different meanings
genHexAddress :: Gen AsmNumeric
genHexAddress = HexLiteral <$> choose (256, maxBound)

genHexByte :: Gen AsmNumeric
genHexByte = HexLiteral <$> choose (0, 255)

genDecimalLiteral :: Gen AsmNumeric
genDecimalLiteral = DecimalLiteral <$> arbitrary

genBinaryLiteral :: Gen AsmNumeric
genBinaryLiteral = BinaryLiteral <$> arbitrary

genNumImmediate :: Gen AsmNumeric
genNumImmediate = oneof [genHexImmediate, genDecImmediate, genBinImmediate]

genDecImmediate :: Gen AsmNumeric
genDecImmediate = DecimalImmediate <$> arbitrary

genHexImmediate :: Gen AsmNumeric
genHexImmediate = HexImmediate <$> arbitrary

genBinImmediate :: Gen AsmNumeric
genBinImmediate = BinaryImmediate <$> arbitrary

instance Arbitrary SourcePos where
    arbitrary = pure SourcePos { sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1 }

instance Arbitrary AsmStatement where
    arbitrary = oneof
        [ StmtDirective <$> arbitrary <*> arbitrary
        , StmtCodeLabel <$> genLabel <*> arbitrary
        , StmtLabelLocation <$> genLabel <*> arbitrary <*> arbitrary
        , StmtDefineVar <$> genVarName <*> genNumLiteral <*> arbitrary
        , StmtProgramLocation <$> genHexLiteral <*> arbitrary
        , StmtOpCode <$> genOpName <*> arbitrary <*> arbitrary
        ]

instance Arbitrary AsmDirectiveType where
    arbitrary = oneof
        [ DtByte <$> listOf genNumLiteral
        , DtDByte <$> listOf genNumLiteral
        , DtWord <$> listOf genNumLiteral
        , DtBlock <$> genDecimalLiteral
        , DtText <$> genRandomText
        , pure DtEnd
        ]

instance Arbitrary AsmNumeric where
    arbitrary = oneof
        [ genHexLiteral
        , genDecimalLiteral
        , genBinaryLiteral
        , genDecImmediate
        , genHexImmediate
        , genBinImmediate
        ]

-- ignore AsmA as the only time we need it is in AsmAddressType
instance Arbitrary AsmRegisterName where
    arbitrary = oneof $ map pure [AsmX, AsmY]

instance Arbitrary AsmAddressType where
    arbitrary = oneof
        [ AsmImmediate <$> genNumImmediate
        , AsmZeroPage <$> genHexByte <*> arbitrary
        , AsmAbsolute <$> genHexAddress <*> arbitrary
        , AsmIndirect <$> genHexLiteral <*> arbitrary
        , AsmRelative <$> choose (-128, 127)
        , pure AsmAccumulator
        , pure AsmImplicit
        ]
