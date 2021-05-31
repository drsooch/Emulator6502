-- | Utility functions that span all tests
module TestUtils
    ( mkTestCPU
    , setTestZeroPage
    , setProgramMemory
    , runEmulatorTest
    ) where


import           Control.Monad.State.Strict     ( runStateT )
import           Data.Array.IArray              ( (//)
                                                , listArray
                                                )
import           Logging
import           System.Directory
import           System.Posix.Files             ( stdFileMode )
import           System.Posix.IO                ( createFile
                                                , fdToHandle
                                                )
import           Types

-- Testing CPU with default values initialized
mkTestCPU :: String -> IO CPUState
mkTestCPU name = do
    logFd <- getCurrentDirectory >>= \cwd -> createFile
        (cwd <> "/log/" <> removeWhiteSpaceFileName name <> "-test.log")
        stdFileMode
    logLocation <- fdToHandle logFd
    let mem        = listArray (0, 0xFFFF) (repeat 0) -- Array of 64kb init to 0
    let memory     = setTestZeroPage mem
    let pc         = PC 0xF000
    let sp         = SP 0x01FF
    let xReg       = Reg 0x0
    let yReg       = Reg 0x0
    let aReg       = Reg 0x0
    let fReg       = Flags 0x0
    let logEnabled = True
    return $ CPU { .. }

removeWhiteSpaceFileName :: String -> String
removeWhiteSpaceFileName = concat . filter (/= "-") . words

-- Set the ZeroPage to have useful information
setTestZeroPage :: Memory -> Memory
setTestZeroPage mem =
    let zp = [ (ix, 0xFF - fromIntegral ix) | ix <- [0 .. 0xFF] ] in mem // zp

setProgramMemory :: [Byte] -> CPUState -> CPUState
setProgramMemory bytes c@CPU {..} =
    let newValues = zip [(getPC pc) ..] bytes
        memory'   = memory // newValues
    in  c { memory = memory' }


runEmulatorTest :: Emulator b -> CPUState -> IO (b, CPUState)
runEmulatorTest f cpu = runStateT f cpu >>= \ret -> closeLog cpu >> return ret
