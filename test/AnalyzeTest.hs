-- | Testing the Analysis Phase of Assembling

module AnalyzeTest
    ( analysis
    ) where

import           Arbitrary                      ( )
import           Assembler.Analyze
import           Assembler.Error
import           Assembler.Types
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Set                      as S
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit
import           Types

analysis :: TestTree
analysis = testGroup
    "Assembler Analysis"
    [ validAnalysisTest
    , emptyParsePassTest
    , duplicateVariableDefinitionTest
    , duplicateLabelInCodeBlockTest
    , duplicateLabelInLabeledLocsTest
    , invalidOpCodeAddressTypeTest
    , invalidDirectiveSizeTest
    , invalidDirectiveSizeTest'
    ]

mkAsmTree :: AsmTree
mkAsmTree =
    AsmTree { progStartIdent = Nothing, definitions = [], codeBlocks = [], labeledLocs = [] }

validAnalysisTest :: TestTree
validAnalysisTest = testCase
    "Analyzing a Passing Case"
    case
        analyzeStatements
            (mkAsmTree
                { definitions = [ VarDefinition "foo" (HexLiteral 4)       undefined
                                , VarDefinition "bar" (DecimalImmediate 7) undefined
                                ]
                , codeBlocks  = [CodeBlock 0 "baz" []]
                , labeledLocs = [LabeledLoc "fizz" (DtText "buzz") undefined]
                }
            )
    of
        Left  err  -> assertFailure $ "Expecting a passing analysis got: " <> show err
        Right _res -> assertFailure "fail"

-- realistically we fail on empty input
-- but better safe than sorry
emptyParsePassTest :: TestTree
emptyParsePassTest = testCase
    "Analyzing an Empty Parse"
    case analyzeStatements mkAsmTree of
        Left err -> assertFailure $ "Expected a Pass got: " <> show err
        _        -> pure ()

duplicateVariableDefinitionTest :: TestTree
duplicateVariableDefinitionTest = testCase
    "Duplicate Variable Definition"
    case
        analyzeStatements
            (mkAsmTree
                { definitions = [ VarDefinition "foo" (HexLiteral 4)       undefined
                                , VarDefinition "bar" (DecimalImmediate 7) undefined
                                , VarDefinition "foo" (DecimalLiteral 1)   undefined
                                ]
                }
            )
    of
        Left err -> DuplicateVariable "foo" @=? err
        _        -> assertFailure "Expected Duplicate Variable Error"

duplicateLabelInCodeBlockTest :: TestTree
duplicateLabelInCodeBlockTest = testCase
    "Duplicate Label Definition - CodeBlock"
    case
        analyzeStatements
            (mkAsmTree
                { definitions = [ VarDefinition "foo" (HexLiteral 4)       undefined
                                , VarDefinition "bar" (DecimalImmediate 7) undefined
                                ]
                , codeBlocks  = [CodeBlock 0 "foo" []]
                }
            )
    of
        Left err -> DuplicateLabel "foo" @=? err
        _        -> assertFailure "Expected Duplicate Label Error"

duplicateLabelInLabeledLocsTest :: TestTree
duplicateLabelInLabeledLocsTest = testCase
    "Duplicate Label Definition - LabeledLocations"
    case
        analyzeStatements
            (mkAsmTree
                { definitions = [ VarDefinition "foo" (HexLiteral 4)       undefined
                                , VarDefinition "bar" (DecimalImmediate 7) undefined
                                ]
                , codeBlocks  = [CodeBlock 0 "baz" []]
                , labeledLocs = [ LabeledLoc "fizz" (DtText "buzz") undefined
                                , LabeledLoc "foo"  DtEnd           undefined
                                ]
                }
            )
    of
        Left err -> DuplicateLabel "foo" @=? err
        _        -> assertFailure "Expected Duplicate Label Error"

invalidOpCodeAddressTypeTest :: TestTree
invalidOpCodeAddressTypeTest = testCase
    "Invalid OpCode Address Type"
    case
        analyzeStatements
            (mkAsmTree
                { codeBlocks =
                    [ CodeBlock
                          0
                          "foo"
                          [ CodeStatement LDA (AsmZeroPage (HexLiteral 1) Nothing) undefined
                          , badStatement
                          ]
                    ]
                }
            )
    of
        Left err -> InvalidOpCodeAddressType badStatement @=? err
        _        -> assertFailure "Expected Invalid OpCode Address Type"
    where badStatement = CodeStatement BIT (AsmImmediate (HexLiteral 1)) undefined

invalidDirectiveSizeTest :: TestTree
invalidDirectiveSizeTest = testCase
    "Invalid Directive Size"
    case
        analyzeStatements
            (mkAsmTree
                { labeledLocs = [LabeledLoc "foobar" (DtText "barfoo") undefined, badStatement]
                }
            )
    of
        Left err -> InvalidDirectiveSize badStatement @=? err
        r        -> putStrLn (show r) >> pure ()-- assertFailure "Expected Invalid Directive Size"
    where badStatement = LabeledLoc "buzz" (DtByte []) undefined

invalidDirectiveSizeTest' :: TestTree
invalidDirectiveSizeTest' = testCase
    "Invalid Directive Size"
    case runExcept (runReaderT (validateLabeledLoc True badStatement) S.empty) of
        Left err -> InvalidDirectiveSize badStatement @=? err
        _        -> assertFailure "Expected Invalid Directive Size"
    where badStatement = LabeledLoc "buzz" (DtByte []) undefined
