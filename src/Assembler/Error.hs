-- | Error Types and handling

module Assembler.Error
    ( AssemblyError(..)
    , assertNotReached
    ) where

import           Assembler.Types
import           Control.Exception              ( assert )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec.Error          ( ParseErrorBundle
                                                , errorBundlePretty
                                                )


type ParserError = ParseErrorBundle Text Void

data AssemblyError = InvalidOpCodeAddressType AsmInstruction
                   | InvalidDirectiveSize LabeledLocation
                   | InvalidVariable Text AsmStatement
                   | DuplicateLabel Text
                   | DuplicateVariable Text
                   | InternalError Text
                   | ParseError ParserError
                   deriving Eq


instance Show AssemblyError where
    show (InvalidOpCodeAddressType cs) =
        "OpCode " <> show (opName cs) <> " contains invalid address type: " <> show (addressType cs)
    show (InvalidDirectiveSize labLoc) =
        "Directive contains invalid size:\n\t" <> show (directType labLoc)
    show (InvalidVariable _ _      ) = "Invalid Variable"
    show (DuplicateLabel    label  ) = "Duplicate Label: " <> unpack label
    show (DuplicateVariable varName) = "Duplicate Variable: " <> unpack varName
    show (InternalError     err    ) = "Internal Error Occurred:\n\t" <> unpack err
    show (ParseError        pErr   ) = "Parsing Error Occurred:\n\t" <> errorBundlePretty pErr

-- used for codepaths that shouldn't be reached to satisfy GHC
assertNotReached :: a
assertNotReached = assert False undefined
