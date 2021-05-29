-- | Display Contents of CPUState and Memory
module Display
    ( showXReg
    , showYReg
    , showAReg
    , showPC
    , showSP
    , showNBytes
    ) where

import qualified Data.Array.IArray             as IA
import           Numeric                        ( showHex )
import           Types

showCPUStateValue :: Display a => (CPUState -> a) -> CPUState -> String
showCPUStateValue v = display . v

showXReg :: CPUState -> String
showXReg = showCPUStateValue xReg

showYReg :: CPUState -> String
showYReg = showCPUStateValue yReg

showAReg :: CPUState -> String
showAReg = showCPUStateValue aReg

showPC :: CPUState -> String
showPC = showCPUStateValue pc

showSP :: CPUState -> String
showSP = showCPUStateValue sp

showNBytes :: Address -> Offset -> CPUState -> [String]
showNBytes addr offset CPU {..} =
    [ showHex i "" <> ": " <> showHex (memory IA.! i) ""
    | i <- [addr .. (addr + offset)]
    ]

-- | Display is simply a Show instance for the User.
class Display a where
  display :: a -> String

instance Display Register where
    display (Reg val) = showHex val ""
instance Display Flags where
    display (Flags val) = showHex val ""

instance Display StackPointer where
    display (SP val) = showHex val ""

instance Display ProgramCounter where
    display (PC val) = showHex val ""
