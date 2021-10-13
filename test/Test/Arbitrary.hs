{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary Instances for Testing
module Test.Arbitrary where

import           Assembler.Types
import           Assembler.Utils
import           Control.Monad                  ( replicateM )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Test.Tasty.QuickCheck          ( Arbitrary
                                                , Gen
                                                , arbitrary
                                                , arbitraryASCIIChar
                                                , choose
                                                , chooseEnum
                                                , frequency
                                                , listOf
                                                , oneof
                                                , sized
                                                )
import           Text.Megaparsec                ( SourcePos(..)
                                                , pos1
                                                )
import           Types                          ( OpName )

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
    arbitrary = pure SourcePos { sourceName = "", sourceLine = pos1, sourceColumn = pos1 }

instance Arbitrary CodeBlock where
    arbitrary = CodeBlock 0 <$> genLabel <*> arbitrary

instance Arbitrary CodeStatement where
    arbitrary = oneof [InstructionStatement <$> arbitrary, ProgramLocStatement <$> arbitrary]

instance Arbitrary AsmInstruction where
    arbitrary = AsmInstruction <$> genOpName <*> arbitrary <*> arbitrary

instance Arbitrary LabeledLocation where
    arbitrary = LabeledLoc <$> genLabel <*> arbitrary <*> arbitrary

instance Arbitrary VarDefinition where
    arbitrary = VarDefinition <$> genVarName <*> genNumLiteral <*> arbitrary

instance Arbitrary ProgramLocation where
    arbitrary = ProgramLoc <$> genHexLiteral <*> arbitrary

instance Arbitrary AsmStatement where
    arbitrary = oneof []

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
        , AsmUnknown <$> oneof [genVarName, genLabel]
        , AsmUnknownImm <$> oneof [genVarName, genLabel]
        ]

instance Arbitrary AsmTree where
    arbitrary = AsmTree <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
