-- | AST for 6502 Assembly

module Assembler.Types.ASMTree
    ( AsmTree(..)
    ) where

import           Assembler.Types.ASMStatement   ( CodeBlock
                                                , LabeledLocation
                                                , ProgramLocation
                                                , VarDefinition
                                                )
import           GHC.Generics                   ( Generic )

data AsmTree = AsmTree
    { definitions    :: [VarDefinition]
    , progStartIdent :: Maybe ProgramLocation
    , codeBlocks     :: [CodeBlock]
    , labeledLocs    :: [LabeledLocation]
    }
    deriving (Show, Generic)
