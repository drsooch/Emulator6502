module Main where

import           Assembler.Parser
import qualified Data.Text.IO                  as TIO
import           Execution                      ( runEmulator )
import           System.Directory
import           System.IO                      ( IOMode(..)
                                                , withFile
                                                )
import           Text.Megaparsec
import           Types

--main :: IO ((), CPUState)
main = do
    parseFile "test/directive.asm"
    -- cwd <- getCurrentDirectory >>= \cwd -> return $ cwd <> "/log/emulator.log"
    -- withFile cwd WriteMode $ \fd -> runEmulator (mkCPU True fd)
