-- | Program Counter Operations

module ProgramCounter
    ( incrementPC
    , setProgramCounter
    , getProgramCounter
    ) where

import           Lens.Micro.Mtl                 ( (%=)
                                                , (.=)
                                                , use
                                                )
import           Types

getProgramCounter :: Emulator Address
getProgramCounter = getPC <$> use #pc

setProgramCounter :: Address -> Emulator ()
setProgramCounter addr = #pc .= PC addr

incrementPC :: (Integral a) => a -> Emulator ()
incrementPC num = #pc %= \pc -> pc + 1 + fromIntegral num
