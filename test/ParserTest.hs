-- | Parsing Test

module ParserTest
    ( parsing
    ) where

import           Assembler.Parser               ( pDirective
                                                , pEndDirective
                                                )
import qualified Data.Text.IO                  as TIO
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( assertBool
                                                , testCase
                                                )
import           Text.Megaparsec                ( parseTest )

parsing :: TestTree
parsing = testGroup "Parsing" [parseDirective]

parseDirective :: TestTree
parseDirective = testCase "Directive Parser" $ do
    contents <- TIO.readFile "test/directive.asm"
    parseTest (pDirective <* pEndDirective) contents
    assertBool "" True
