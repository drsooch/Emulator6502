-- | Display Contents of CPU and Memory
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

showCPUValue :: Display a => (CPU -> a) -> CPU -> String
showCPUValue v = display . v

showXReg :: CPU -> String
showXReg = showCPUValue xReg

showYReg :: CPU -> String
showYReg = showCPUValue yReg

showAReg :: CPU -> String
showAReg = showCPUValue aReg

showPC :: CPU -> String
showPC = showCPUValue pc

showSP :: CPU -> String
showSP = showCPUValue sp

showNBytes :: Address -> Offset -> CPU -> [String]
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
