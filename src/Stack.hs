-- | Stack Operations

module Stack
    ( incrementSP
    , decrementSP
    , popStackByte
    , pushStackByte
    , popStackAddress
    , pushStackAddress
    , setStackPointer
    , getStackPointer
    ) where

import           Lens.Micro.Mtl                 ( (.=)
                                                , use
                                                )
import           Memory
import           Types
import           Utils

getStackPointer :: Emulator Address
getStackPointer = getSP <$> use #sp

setStackPointer :: Byte -> Emulator ()
setStackPointer val = #sp .= SP (0x100 + fromIntegral val)

-- this allows for wrapping on the stack i.e. 0x1FF + push = 0x100
incrementSP :: Emulator ()
incrementSP = getStackPointer >>= setStackPointer . (+) 1 . fromIntegral

-- this allows for wrapping on the stack i.e. 0x100 + pull/pop = 0x1FF
decrementSP :: Emulator ()
decrementSP = getStackPointer >>= setStackPointer . (-) 1 . fromIntegral

-- pop value from stack and increment pointer
popStackByte :: Emulator Byte
popStackByte = getStackPointer >>= \sp -> incrementSP >> fetchByte sp

-- push a value on to stack and decrement pointer
pushStackByte :: Byte -> Emulator ()
pushStackByte byte = getStackPointer >>= flip setMemory [byte] >> decrementSP

-- pop address from stack lsb first
popStackAddress :: Emulator Address
popStackAddress = do
    -- get the current stack pointer to LSB
    spLsb <- getStackPointer
    -- get that LSB
    lsb   <- fetchByte spLsb
    -- increment stack pointer which wraps around the stack (just in case)
    incrementSP
    -- get the current stack pointer to MSB
    spMsb <- getStackPointer
    msb   <- fetchByte spMsb
    incrementSP
    return $ toAddress lsb msb

-- push an address on to stack, msb first
pushStackAddress :: Address -> Emulator ()
pushStackAddress addr = do
    spMsb <- getStackPointer
    decrementSP
    let (lsb, msb) = fromAddress addr
    setMemory spMsb [msb]
    decrementSP
    spLsb <- getStackPointer
    setMemory spLsb [lsb]
