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
    , logInstruction
    , logCPUState
    ) where

import           Control.Monad                  ( when )
import           Control.Monad.State.Strict     ( MonadIO(liftIO)
                                                , get
                                                , gets
                                                , void
                                                )
import           Display
import           Lens.Micro.Mtl                 ( use )
import           System.IO
import           Types

closeLog :: CPUState -> IO ()
closeLog cpu = do
    isOpen <- hIsOpen $ logLocation cpu
    when isOpen $ hClose (logLocation cpu)

writeLog :: Display a => a -> Emulator ()
writeLog msg = use #logLocation >>= liftIO . flip hPutStrLn (display msg)

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

logInstruction :: Instruction -> Emulator ()
logInstruction = writeLog

logCPUState :: Emulator ()
logCPUState = get >>= writeLog
