-- | Manipulating the Flag Register
module Flags
    ( FlagType(..)
    ,
    -- functions
      updateFlag
    , isFlagSet
    , isNegative
    , isOverflow
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
                                                )
import           Emulator
import           Lens.Micro.Mtl

data FlagType = CF | ZF | IF | DF | BF | OF | NF
  deriving (Eq, Show)

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


setFlag :: FlagType -> Flags -> Flags
setFlag ft flags = flags .|. toBitRep ft

clearFlag :: FlagType -> Flags -> Flags
clearFlag ft flags = flags .&. complement (toBitRep ft)

updateFlag :: Bool -> FlagType -> Emulator ()
updateFlag bool ft = #fReg %= updateFlag' bool ft

updateFlag' :: Bool -> FlagType -> Flags -> Flags
updateFlag' True  = setFlag
updateFlag' False = clearFlag

isFlagSet :: FlagType -> Emulator Bool
isFlagSet fType =
    use #fReg >>= \flags -> return $ isFlagSet' flags (toBitNum fType)

isFlagSet' :: Flags -> Int -> Bool
isFlagSet' = testBit

isNegative :: Byte -> Bool
isNegative val = isFlagSet' (Flags val) (toBitNum NF)

isOverflow :: Byte -> Bool
isOverflow val = isFlagSet' (Flags val) (toBitNum OF)
