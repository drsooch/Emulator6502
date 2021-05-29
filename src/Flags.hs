-- | Manipulating the Flag Register
module Flags
    (
    -- functions
      setFRegister
    , getFRegister
    , setFlag
    , clearFlag
    , updateFlag
    , isFlagSet
    , isNegative
    , isOverflow
    , isZero
    , isCarryAdd
    , isCarrySub
    ,
    -- Constants
      carryFlag
    , zeroFlag
    , intFlag
    , decFlag
    , brkFlag
    , ovFlag
    , negFlag
    ) where

import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , Bits
                                                , complement
                                                , testBit
                                                , xor
                                                )
import           Lens.Micro.Mtl
import           Types


-- Bits for individual flags
carryFlag, zeroFlag, intFlag, decFlag, brkFlag, ovFlag, negFlag
    :: (Num a, Bits a) => a
carryFlag = 0x1
zeroFlag = 0x2
intFlag = 0x8
decFlag = 0x10
brkFlag = 0x20
ovFlag = 0x40
negFlag = 0x80

toBitRep :: (Num a, Bits a) => FlagType -> a
toBitRep = \case
    CF -> carryFlag
    ZF -> zeroFlag
    IF -> intFlag
    DF -> decFlag
    BF -> brkFlag
    OF -> ovFlag
    NF -> negFlag

toBitNum :: FlagType -> Int
toBitNum = \case
    CF -> 1
    ZF -> 2
    IF -> 3
    DF -> 4
    BF -> 5
    OF -> 6
    NF -> 7


getFRegister :: Emulator Byte
getFRegister = getFlags <$> use #fReg

setFRegister :: Byte -> Emulator ()
setFRegister byte = #fReg .= Flags byte

setFlag :: FlagType -> Emulator ()
setFlag ft = #fReg %= \flags -> flags .|. toBitRep ft

clearFlag :: FlagType -> Emulator ()
clearFlag ft = #fReg %= \flags -> flags .&. complement (toBitRep ft)

setFlag' :: FlagType -> Flags -> Flags
setFlag' ft flags = flags .|. toBitRep ft

clearFlag' :: FlagType -> Flags -> Flags
clearFlag' ft flags = flags .&. complement (toBitRep ft)

updateFlag :: Bool -> FlagType -> Emulator ()
updateFlag True  ft = #fReg %= setFlag' ft
updateFlag False ft = #fReg %= clearFlag' ft

isFlagSet :: FlagType -> Emulator Bool
isFlagSet fType =
    use #fReg >>= \flags -> return $ isFlagSet' flags (toBitNum fType)

isFlagSet' :: Flags -> Int -> Bool
isFlagSet' = testBit

-- Boolean Operations for checking whether to set a bit or not in Flags.
-- Generally used in conjuction with `updateFlag`.

-- check if result is negative (7th bit)
isNegative :: Byte -> Bool
isNegative = flip testBit 7

-- Checks to see if overflow occurs in arithmetic operation
-- LHS ^ RESULT -> checks if sign bit differs
-- RHS ^ RESULT -> checks if sign bit differs
-- AND the results above, this checks if both LHS and RHS had differing signs from RESULT
-- MASK out the top bit, if not equal to 0 we have overflow
isOverflow :: Byte -> Byte -> Byte -> Bool
isOverflow lhs rhs res =
    (((lhs `xor` res) .&. (rhs `xor` res)) .&. 0x80) /= 0x0

-- Guess what this does? ;)
isZero :: Byte -> Bool
isZero = (==) 0

-- Check to see if result is less than the accumulator
-- must capture the current carry bit
isCarryAdd :: Byte -> Byte -> Byte -> Bool
isCarryAdd acc result 1 = result <= acc
isCarryAdd acc result 0 = result < acc
isCarryAdd _   _      _ = error "isCarry received an invalid Carry Bit"

-- Carry is set when accumulator is GTEQ to the value to subtract
-- "Quirk" of the SBC instruction, I think...
isCarrySub :: Byte -> Byte -> Bool
isCarrySub acc subber = acc >= subber
