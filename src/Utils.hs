-- | Utility Functions for the Emulator

module Utils
    ( shiftR
    , shiftL
    , toByte
    , fetchByte
    , fetchAddress
    , toAddress
    , fromAddress
    ) where

import qualified Data.Array.IArray             as IA
import qualified Data.Bits                     as B
import           Lens.Micro.Mtl                 ( use )
import           Types

-- We have to do this so we can use the proper signature in our Execution file
-- Bits.shift uses Int as the number to shift by
shiftL :: Byte -> Byte -> Byte
shiftL lhs shift = lhs `B.shiftL` fromIntegral shift

shiftR :: Byte -> Byte -> Byte
shiftR lhs shift = lhs `B.shiftR` fromIntegral shift

toByte :: Address -> Byte
toByte = fromIntegral

fetchByte :: Address -> Emulator Byte
fetchByte addr = use #memory >>= \mem -> return $ mem IA.! addr

fetchAddress :: Address -> Emulator Address
fetchAddress addr = do
    mem <- use #memory
    return $ toAddress (mem IA.! addr) (mem IA.! (addr + 1))

-- convert two bytes into a 2 byte address
-- Little Endian
toAddress :: Byte -> Byte -> Address
toAddress lsb msb = (fromIntegral msb) `B.shiftL` 8 + fromIntegral lsb

-- fromIntegral addr masks out top bits
fromAddress :: Address -> (Byte, Byte)
fromAddress addr = (fromIntegral addr, fromIntegral $ addr `B.shiftR` 8)
