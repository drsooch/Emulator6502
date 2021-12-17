-- | Testing the Analysis Phase of Assembling
module Test.Assembler.AnalyzeTest
    ( analysis
    ) where

import           Test.Arbitrary                      ( )
import           Assembler.Analyze
import           Assembler.Error
import           Assembler.Types
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Set                      as S
import           Data.Word                      ( Word16 )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           TestUtils
import           Text.Megaparsec.Pos            ( pos1 )
import           Types

analysis :: TestTree
analysis = testGroup
    "Assembler Analysis"
    [ validAnalysisTest
    , emptyParsePassTest
    , duplicateVariableDefinitionTest
    , duplicateCodeBlockTest
    , duplicateLabeledLocsTest
    , duplicateLabelInCodeBlockAndDefTest
    , duplicateLabelInDefAndLabeledLocTest
    , invalidOpCodeAddressTypeTest
    , invalidDirectiveSizeTest
    ]


validAnalysisTest :: TestTree
validAnalysisTest = testCase
    "Valid Analysis"
    case analyzeStatements (AsmTree varDs Nothing [cbs] lls) of
        Left  err  -> assertFailure $ "Expecting a passing analysis got: " <> show err
        Right _res -> pure ()
  where
    varD1 = var "var1" (hexImm 46)
    varD2 = var "var2" (hexImm 12)
    varDs = [varD1, varD2]
    cb1   = asmInst ADC (asmImm (hexImm 12))
    cb2   = asmInst STA (AsmZeroPage (hexImm 12) Nothing)
    cb3   = asmInst ROR AsmAccumulator
    cb4   = asmInst JSR (AsmAbsolute (hexImm 5700) Nothing)
    cbs   = codeBlock "block1" [cb1, cb2, cb3, cb4]
    ll1   = labeledLoc "ll1" (DtByte [HexLiteral 12])
    lls   = [ll1]

-- realistically we fail on empty input
-- but better safe than sorry
emptyParsePassTest :: TestTree
emptyParsePassTest = testCase
    "Analyzing an Empty Parse"
    case analyzeStatements mkEmptyAsmTree of
        Left err -> assertFailure $ "Expected a Pass got: " <> show err
        _        -> pure ()

duplicateVariableDefinitionTest :: TestTree
duplicateVariableDefinitionTest = testCase
    "Duplicate Variable Definition"
    let tree = (mkDefAsmTree [var "foo" (hexLit 4), var "bar" (decImm 7), var "foo" (decLit 1)])
    in  case analyzeStatements tree of
            Left err -> DuplicateBinding "foo" @=? err
            _        -> assertFailure "Expected Duplicate Variable Error"

duplicateCodeBlockTest :: TestTree
duplicateCodeBlockTest = testCase
    "Duplicate Code Block Label"
    let tree = (mkCBAsmTree [codeBlock "foo" [], codeBlock "bar" [], codeBlock "foo" []])
    in  case analyzeStatements tree of
            Left err -> DuplicateBinding "foo" @=? err
            _        -> assertFailure "Expected Duplicate Label Error"

duplicateLabeledLocsTest :: TestTree
duplicateLabeledLocsTest = testCase
    "Duplicate Labeled Location Label"
    let tree =
            (mkLLAsmTree
                [ labeledLoc "foo"  (DtByte [hexLit 1])
                , labeledLoc "fizz" (DtText "buzz")
                , labeledLoc "foo"  DtEnd
                ]
            )
    in  case analyzeStatements tree of
            Left err -> DuplicateBinding "foo" @=? err
            _        -> assertFailure "Expected Duplicate Label Error"

duplicateLabelInCodeBlockAndDefTest :: TestTree
duplicateLabelInCodeBlockAndDefTest = testCase
    "Duplicate Label Definition - CodeBlock and Var Def"
    let tree =
            (mkAsmTree Nothing
                       [var "foo" (HexLiteral 4), var "bar" (DecimalImmediate 7)]
                       [codeBlock "buzz" [], codeBlock "foo" []]
                       []
            )
    in  case analyzeStatements tree of
            Left err -> DuplicateBinding "foo" @=? err
            _        -> assertFailure "Expected Duplicate Label Error"

duplicateLabelInDefAndLabeledLocTest :: TestTree
duplicateLabelInDefAndLabeledLocTest = testCase
    "Duplicate Label Definition - LabeledLoc and Var Def"
    let tree =
            (mkAsmTree Nothing
                       [var "foo" (HexLiteral 4), var "bar" (DecimalImmediate 7)]
                       []
                       [labeledLoc "fizz" (DtText "buzz"), labeledLoc "foo" DtEnd]
            )
    in  case analyzeStatements tree of
            Left err -> DuplicateBinding "foo" @=? err
            _        -> assertFailure "Expected Duplicate Label Error"


invalidOpCodeAddressTypeTest :: TestTree
invalidOpCodeAddressTypeTest = testCase
    "Invalid OpCode Address Type"
    case
        analyzeStatements
            (mkEmptyAsmTree
                { codeBlocks = [ codeBlock
                                     "foo"
                                     [ asmInst LDA (AsmZeroPage (HexLiteral 1) Nothing)
                                     , InstructionStatement badStatement
                                     ]
                               ]
                }
            )
    of
        Left err -> InvalidOpCodeAddressType badStatement @=? err
        _        -> assertFailure "Expected Invalid OpCode Address Type"
    where badStatement = AsmInstruction BIT (AsmImmediate (HexLiteral 1)) undefined

invalidDirectiveSizeTest :: TestTree
invalidDirectiveSizeTest = testCase
    "Invalid Directive Size"
    case
        analyzeStatements
            (mkEmptyAsmTree
                { labeledLocs = [LabeledLoc "foobar" (DtText "barfoo") undefined, badStatement]
                }
            )
    of
        Left err -> InvalidDirectiveSize badStatement @=? err
        _        -> assertFailure "Expected Invalid Directive Size"
    where badStatement = LabeledLoc "buzz" (DtByte []) undefined
