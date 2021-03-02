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
import           Emulator
import           Numeric                        ( showHex )

showCPUValue :: Show a => (CPU -> a) -> CPU -> String
showCPUValue f = show . f

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
