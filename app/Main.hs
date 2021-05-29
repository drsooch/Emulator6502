module Main where

import           Execution
import           Types

main :: IO ((), CPUState, [String])
main = runEmulator $ hardResetCPU mkCPU
