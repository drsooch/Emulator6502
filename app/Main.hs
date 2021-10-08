module Main where

import           Assembler.Analyze
import           Assembler.Parser
import           Assembler.Types
import           Assembler.Types.Pretty         ( renderStatements )
import qualified Data.Text.IO                  as TIO
import           Text.Megaparsec.Pos
import           Types

main :: IO ()
main = do
    -- result <- parseFile "test/files/directive.asm"
    -- case result of
    --     Left  err -> putStrLn $ errorBundlePretty err
    --     Right r   -> print r
    let fakeSource = SourcePos { sourceName = "", sourceLine = pos1, sourceColumn = pos1 }
    let vd = [VarDefinition { varName = "eE", value = HexLiteral 1, sourcePos = fakeSource }]
    let
        cb =
            [ CodeBlock
                  { progOffset = 0
                  , blockLabel = "Hu"
                  , statements =
                      [ InstructionStatement
                          (AsmInstruction RTS
                                          (AsmAbsolute (HexLiteral 13966) (Just AsmX))
                                          fakeSource
                          )
                      , InstructionStatement
                          (AsmInstruction BVS (AsmImmediate (BinaryImmediate 1)) fakeSource)
                      ]
                  }
            ]
    let ll =
            [ LabeledLoc
                  { locLabel   = "Ua"
                  , directType = DtWord [DecimalLiteral 2, HexLiteral 2]
                  , sourcePos  = SourcePos { sourceName   = ""
                                           , sourceLine   = pos1
                                           , sourceColumn = pos1
                                           }
                  }
            ]
    let
        stmt =
            renderStatements vd
                <> ".CODE\n"
                <> renderStatements cb
                <> ".DATA\n"
                <> renderStatements ll
    case parseAssembly "test" stmt of
        Left  e -> print e
        Right _ -> print "success"
    -- case analyzeStatements (AsmTree [] Nothing stmt []) of
    --     Left  e      -> print e
    --     Right result -> TIO.putStrLn (renderStatements stmt) >> print "Success" >> print result

    -- cwd <- getCurrentDirectory >>= \cwd -> return $ cwd <> "/log/emulator.log"
    -- withFile cwd WriteMode $ \fd -> runEmulator (mkCPU True fd)
