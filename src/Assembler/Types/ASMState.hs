-- | Types for dealing with State while analyzing and code generation
module Assembler.Types.ASMState
  ( AsmState(..)
  , Bindings(..)
  , AssemblyExe(..)
  , ByteType(..)
  , Assembler
  , CodeLabels
  , MemLocations
  , Variables
  ) where

import           Assembler.Types.ASMStatement   ( AsmNumeric )
import           Assembler.Types.ASMTree        ( AsmTree )
import           Control.Monad.State.Strict     ( State )
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Data.Word                      ( Word16
                                                , Word8
                                                )
import           GHC.Generics                   ( Generic )

-- Map Defined Vars to their Value
type Variables = Map Text AsmNumeric

-- Map Labeled Memory Locations to a size in the data section
-- (Memory Offset, Size of Memory)
type MemLocations = Map Text (Int, Int)

-- Map Code Label to an Address (unintialized to start)
type CodeLabels = Map Text Word16

type Assembler a = State AsmState a

data AsmState = AsmState
  { codeOffset :: Int
  , dataOffset :: Int
  , asmTree    :: AsmTree
  , bindings   :: Bindings
  }
  deriving Generic

data Bindings = Bindings
  { variables   :: Variables
  , codeLabels  :: CodeLabels
  , labeledLocs :: MemLocations
  }
  deriving (Show, Generic)

data AssemblyExe = Executable
  { dataBytes :: [ByteType]    -- "Data" section
  , codeBytes :: [ByteType]    -- "Code" section
  , progStart :: Maybe Word16  -- Where to load the code
  }
  deriving Show

data ByteType = Literal Word8
              | Label Text
              | Offset Int [Word8]
              deriving (Show)
