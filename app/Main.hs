module Main where

import           Emulator
import           Execution

main :: IO ((), CPU, [String])
main = runEmulator $ hardResetCPU mkCPU
