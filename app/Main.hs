module Main where

import           Execution                      ( runEmulator )
import           System.Directory
import           System.IO                      ( IOMode(..)
                                                , withFile
                                                )
import           Types

main :: IO ((), CPUState)
main = do
    cwd <- getCurrentDirectory >>= \cwd -> return $ cwd <> "/log/emulator.log"
    withFile cwd WriteMode $ \fd -> runEmulator (mkCPU True fd)
