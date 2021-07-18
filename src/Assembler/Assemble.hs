-- | Assemble the parsed instructions

module Assembler.Assemble
    ( assemble
    ) where

import           Assembler.Analyze
import           Assembler.Error
import           Assembler.Types

-- Construct an executable from a parsed ASM file
assemble :: AsmTree -> Either AssemblyError AssemblyExe
assemble = undefined <$> mkAsmState
-- assemble = evalState mkExecutable <$> mkAsmState

-- Make our starting Assembler State
-- will make one pass to analyze the statements for validaty and label/variable capture
mkAsmState :: AsmTree -> Either AssemblyError AsmState
mkAsmState tree = AsmState 0 0 tree <$> analyzeStatements tree

-- Emit the actual bytes for the executable
mkExecutable :: Assembler AssemblyExe
mkExecutable = undefined
--    emitDataBytes >>= \db -> emitCodeBytes >>= \cb -> Executable db cb <$> getProgramStart
