-- | Register Operations

module Register
    ( setXRegister
    , setYRegister
    , setARegister
    , getXRegister
    , getYRegister
    , getARegister
    , applyRegister
    , applyARegister
    , applyXRegister
    , applyYRegister
    ) where

import           Lens.Micro.Mtl                 ( (.=)
                                                , (<%=)
                                                , use
                                                )
import           Types

-- Set Specific registers and return the result
-- (Most of the time the result is discarded)
setARegister :: Byte -> Emulator Byte
setARegister byte = #aReg .= Reg byte >> return byte

setXRegister :: Byte -> Emulator Byte
setXRegister byte = #xReg .= Reg byte >> return byte

setYRegister :: Byte -> Emulator Byte
setYRegister byte = #yReg .= Reg byte >> return byte

-- Gets a register and unwraps it
getARegister :: Emulator Byte
getARegister = getReg <$> use #aReg

getXRegister :: Emulator Byte
getXRegister = getReg <$> use #xReg

getYRegister :: Emulator Byte
getYRegister = getReg <$> use #yReg

-- Applies a function to the register and returns result
applyRegister
    :: RegisterName
    -> (Register -> Register -> Register)
    -> Byte
    -> Emulator Byte
applyRegister reg f val = getReg <$> (reg <%= f (Reg val))

applyARegister :: (Byte -> Byte -> Byte) -> Byte -> Emulator Byte
-- applyARegister f val = do
--     aReg <- getARegister
--     let result = aReg `f` val
--     setARegister result
 -- (#aReg <%= onRegister f (Reg val)) >> getARegister
applyARegister f val = getReg <$> (#aReg <%= onRegister f (Reg val))


applyXRegister :: (Byte -> Byte -> Byte) -> Byte -> Emulator Byte
applyXRegister f val = getReg <$> (#xReg <%= onRegister f (Reg val))

applyYRegister :: (Byte -> Byte -> Byte) -> Byte -> Emulator Byte
applyYRegister f val = getReg <$> (#yReg <%= onRegister f (Reg val))

onRegister :: (Byte -> Byte -> Byte) -> Register -> Register -> Register
onRegister f (Reg rhs) (Reg lhs) = Reg $ f lhs rhs
