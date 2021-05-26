-- | Program Counter Operations

module ProgramCounter
    ( incrementPC
    ) where

import           Lens.Micro.Mtl                 ( (%=) )
import           Types                          ( Emulator )

incrementPC :: Int -> Emulator ()
incrementPC num = #pc %= (+) (1 + fromIntegral num)
