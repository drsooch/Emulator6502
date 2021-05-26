-- | Stack Operations

module Stack
    ( incrementSP
    , decrementSP
    , popStack
    , pushStack
    , spInRangeM
    ) where

import qualified Data.Array.IArray             as IA
import           Lens.Micro.Mtl                 ( (%=)
                                                , (+=)
                                                , (-=)
                                                , use
                                                )
import           Types

incrementSP :: Emulator ()
incrementSP = #sp += 1

decrementSP :: Emulator ()
decrementSP = #sp -= 1

-- pop value from stack and increment pointer
popStack :: Emulator Byte
popStack = do
    (SP sp') <- use #sp
    mem      <- use #memory
    incrementSP
    return $ mem IA.! sp'

-- push a value on to stack and decrement pointer
pushStack :: Byte -> Emulator ()
pushStack byte = do
    (SP sp') <- use #sp
    #memory %= (\m -> m IA.// [(sp', byte)])
    decrementSP

-- TODO: Change return value to not be out of place?
spInRangeM :: Emulator CPUState
spInRangeM = do
    sp' <- use #sp
    return $ if
        | spBelowFloor sp'   -> CPUError StackOverflow
        | spAboveCeiling sp' -> CPUError StackUnderflow
        | otherwise          -> Running


-- spInRange :: StackPointer -> Bool
-- spInRange sp = spAboveFloor sp && spBelowCeiling sp

spBelowFloor :: StackPointer -> Bool
spBelowFloor (SP sp) = sp < 0x0100

spAboveCeiling :: StackPointer -> Bool
spAboveCeiling (SP sp) = sp > 0x01FF
