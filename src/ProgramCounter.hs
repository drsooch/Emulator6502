-- | Program Counter Operations

module ProgramCounter
    ( incrementPC
    , setProgramCounter
    , getProgramCounter
    , offsetProgramCounter
    ) where

import           Lens.Micro.Mtl                 ( (%=)
                                                , (.=)
                                                , use
                                                )
import           Types

offsetProgramCounter :: (Integral a, Num a) => a -> Emulator ()
offsetProgramCounter offset = #pc %= \pc -> pc + fromIntegral offset

getProgramCounter :: Emulator Address
getProgramCounter = getPC <$> use #pc

setProgramCounter :: Address -> Emulator ()
setProgramCounter addr = #pc .= PC addr

incrementPC :: (Integral a) => a -> Emulator ()
incrementPC num = #pc %= \pc -> pc + 1 + fromIntegral num
