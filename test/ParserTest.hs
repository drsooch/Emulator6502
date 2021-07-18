-- | Parsing Test

module ParserTest
    ( parsing
    ) where

import           Arbitrary                      ( )
import           Assembler.Parser
import           Assembler.Types
import           Data.Text                      ( pack )
import           Test.QuickCheck
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.QuickCheck
import           Text.Megaparsec                ( eof
                                                , manyTill
                                                , runParser
                                                )
import           Text.PrettyPrint               ( render )
import           Text.PrettyPrint.HughesPJClass ( pPrint )

parsing :: TestTree
parsing = testGroup
    "Parsing"
    [parseDefines, parseCodeBlocks, parseLabeledLocs, parseProgramLoc, parseProgramFull]

parseDefines :: TestTree
parseDefines = testProperty "Defines" definesTest

parseCodeBlocks :: TestTree
parseCodeBlocks = testProperty "Code Blocks" codeBlocksTest

parseLabeledLocs :: TestTree
parseLabeledLocs = testProperty "Labeled Locations" labeledLocsTest

parseProgramLoc :: TestTree
parseProgramLoc = testProperty "Program Location" programLocTest

parseProgramFull :: TestTree
parseProgramFull = testProperty "Full Program" programFullTest

programFullTest :: [VarDefinition] -> [CodeBlock] -> [LabeledLocation] -> Property
programFullTest defs code memloc =
    not (null defs)
        ==> not (null code)
        ==> not (null memloc)
        ==> either failcase check
        $   parseAssembly
                "Full Program Parser"
                (renderStatements defs <> renderStatements code <> renderStatements memloc)
  where
    failcase _ = property False
    check AsmTree {..} = defs === definitions .&&. code === codeBlocks .&&. memloc === labeledLocs

definesTest :: [VarDefinition] -> Property
definesTest stmts =
    case runParser (manyTill pDefineVar eof) "Define Variable Parser" (renderStatements stmts) of
        Left  _      -> property False
        Right result -> stmts === result

codeBlocksTest :: [CodeBlock] -> Property
codeBlocksTest stmts =
    case runParser (manyTill pCodeBlockTest eof) "Code Block Parser" (renderStatements stmts) of
        Left  _      -> property False
        Right result -> stmts === result

labeledLocsTest :: [LabeledLocation] -> Property
labeledLocsTest stmts =
    case runParser pLabeledLocations "Labeled Location Parser" (renderStatements stmts) of
        Left  _      -> property False
        Right result -> stmts === result

programLocTest :: ProgramLocation -> Property
programLocTest stmts =
    case runParser pProgramLocation "Labeled Location Parser" (pack $ render $ pPrint stmts) of
        Left  _      -> property False
        Right result -> stmts === result
