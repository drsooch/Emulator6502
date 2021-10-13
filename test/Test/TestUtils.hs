-- | Utility functions that span all tests
module TestUtils where

import           Assembler.Types
import           Control.Monad.State.Strict     ( runStateT )
import           Data.Array.IArray              ( (//)
                                                , listArray
                                                )
import qualified Data.Array.IArray             as IA
import           Data.Text                      ( Text
                                                , concat
                                                , filter
                                                , unpack
                                                , words
                                                )
import           Data.Word                      ( Word16 )
import           Logging
import           Prelude                 hiding ( concat
                                                , filter
                                                , words
                                                )
import           System.Directory
import           System.Posix.Files             ( stdFileMode )
import           System.Posix.IO                ( createFile
                                                , fdToHandle
                                                )
import           Types

-- Testing CPU with default values initialized
mkTestCPU :: Text -> IO CPUState
mkTestCPU name = do
    logFd <- getCurrentDirectory >>= \cwd -> createFile
        (cwd <> "/log/" <> unpack (removeWhiteSpaceFileName name) <> "-test.log")
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

removeWhiteSpaceFileName :: Text -> Text
removeWhiteSpaceFileName = concat . words . filter (/= '-')

-- Set the ZeroPage to have useful information
setTestZeroPage :: Memory -> Memory
setTestZeroPage mem = let zp = [ (ix, 0xFF - fromIntegral ix) | ix <- [0 .. 0xFF] ] in mem // zp

setProgramMemory :: [Byte] -> CPUState -> CPUState
setProgramMemory bytes c@CPU {..} =
    let newValues = zip [(getPC pc) ..] bytes
        memory'   = memory // newValues
    in  c { memory = memory' }

setTestMemory :: Address -> [Byte] -> CPUState -> CPUState
setTestMemory addr bytes cpu =
    let newValues = zip [addr ..] bytes
        memory'   = memory cpu // newValues
    in  cpu { memory = memory' }

setARegisterTest :: Byte -> CPUState -> CPUState
setARegisterTest aVal cpu = cpu { aReg = Reg aVal }

setXRegisterTest :: Byte -> CPUState -> CPUState
setXRegisterTest xVal cpu = cpu { xReg = Reg xVal }

setYRegisterTest :: Byte -> CPUState -> CPUState
setYRegisterTest yVal cpu = cpu { yReg = Reg yVal }

setFRegisterTest :: Byte -> CPUState -> CPUState
setFRegisterTest fVal cpu = cpu { fReg = Flags fVal }

setStackPointerTest :: Address -> CPUState -> CPUState
setStackPointerTest addr cpu = cpu { sp = SP addr }

getTestMemoryLoc :: Address -> CPUState -> Byte
getTestMemoryLoc addr cpu = memory cpu IA.! addr

runEmulatorTest :: Emulator b -> CPUState -> IO (b, CPUState)
runEmulatorTest f cpu = runStateT f cpu >>= \ret -> closeLog cpu >> return ret

var :: Text -> AsmNumeric -> VarDefinition
var name num = VarDefinition name num undefined

hexImm, hexLit, decImm, decLit :: Word16 -> AsmNumeric
hexImm = HexImmediate
hexLit = HexLiteral
decImm = DecimalImmediate
decLit = DecimalLiteral

asmInst :: OpName -> AsmAddressType -> CodeStatement
asmInst op mem = InstructionStatement (AsmInstruction op mem undefined)

asmImm :: AsmNumeric -> AsmAddressType
asmImm = AsmImmediate

codeBlock :: Text -> [CodeStatement] -> CodeBlock
codeBlock = CodeBlock 0

labeledLoc :: Text -> AsmDirectiveType -> LabeledLocation
labeledLoc name direct = LabeledLoc name direct undefined

mkEmptyAsmTree :: AsmTree
mkEmptyAsmTree = mkAsmTree Nothing [] [] []

mkDefAsmTree :: [VarDefinition] -> AsmTree
mkDefAsmTree defs = mkAsmTree Nothing defs [] []

mkCBAsmTree :: [CodeBlock] -> AsmTree
mkCBAsmTree cb = mkAsmTree Nothing [] cb []

mkLLAsmTree :: [LabeledLocation] -> AsmTree
mkLLAsmTree ll = mkAsmTree Nothing [] [] ll

mkAsmTree
    :: Maybe ProgramLocation -> [VarDefinition] -> [CodeBlock] -> [LabeledLocation] -> AsmTree
mkAsmTree psi def cb ll =
    AsmTree { progStartIdent = psi, definitions = def, codeBlocks = cb, labeledLocs = ll }
