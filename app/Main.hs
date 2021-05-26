module Main where

import           Execution
import           Types

main :: IO ((), CPU, [String])
main = runEmulator $ hardResetCPU mkCPU
