module Main where

import           Assembler.Parser
import           Assembler.Types
import           Control.Monad                  ( forM_ )
import           Data.List                      ( subsequences )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )
import           Execution                      ( runEmulator )
import           System.Directory
import           System.IO                      ( IOMode(..)
                                                , withFile
                                                )
import           Text.Megaparsec                ( SourcePos(..)
                                                , eof
                                                , errorBundlePretty
                                                , manyTill
                                                , mkPos
                                                , pos1
                                                , runParser
                                                )
import           Types

main :: IO ()
main = do
    -- result <- parseFile "test/files/directive.asm"
    -- case result of
    --     Left  err -> putStrLn $ errorBundlePretty err
    --     Right r   -> print r
    let stmt = [] :: [VarDefinition]
    case parseAssembly "Testing" (renderStatements stmt) of
        Left  e      -> print e
        Right result -> TIO.putStrLn (renderStatements stmt) >> print "Success" >> print result

    -- cwd <- getCurrentDirectory >>= \cwd -> return $ cwd <> "/log/emulator.log"
    -- withFile cwd WriteMode $ \fd -> runEmulator (mkCPU True fd)
