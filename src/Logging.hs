-- | Logging information for Emulation

module Logging
    ( closeLog
    , writeLog
    , logARegister
    , logXRegister
    , logYRegister
    , logFlags
    , logStackPointer
    , logProgramCounter
    , logMemoryLocation
    , logSingleMemoryLocation
    , logAddressInMemory
    , logInstruction
    , logCPUState
    ) where

import           Control.Monad                  ( when )
import           Control.Monad.State.Strict     ( MonadIO(liftIO)
                                                , get
                                                )
import qualified Data.Text.IO                  as TIO
import           Display
import           Lens.Micro.Mtl                 ( use )
import           System.IO
import           Types

closeLog :: CPUState -> IO ()
closeLog cpu = do
    isOpen <- hIsOpen $ logLocation cpu
    when isOpen $ hClose (logLocation cpu)

writeLog :: Display a => a -> Emulator ()
writeLog msg = use #logLocation >>= liftIO . flip TIO.hPutStrLn (display msg)

logARegister :: Emulator ()
logARegister = use #aReg >>= writeLog

logXRegister :: Emulator ()
logXRegister = use #xReg >>= writeLog

logYRegister :: Emulator ()
logYRegister = use #yReg >>= writeLog

logFlags :: Emulator ()
logFlags = use #fReg >>= writeLog

logStackPointer :: Emulator ()
logStackPointer = use #sp >>= writeLog

logProgramCounter :: Emulator ()
logProgramCounter = use #pc >>= writeLog

logMemoryLocation :: Address -> Offset -> Emulator ()
logMemoryLocation addr offset = get >>= mapM_ writeLog . showNBytes addr offset

logSingleMemoryLocation :: Address -> Emulator ()
logSingleMemoryLocation = flip logMemoryLocation 0

logAddressInMemory :: Address -> Emulator ()
logAddressInMemory = flip logMemoryLocation 1

logInstruction :: Instruction -> Emulator ()
logInstruction = writeLog

logCPUState :: Emulator ()
logCPUState = get >>= writeLog
