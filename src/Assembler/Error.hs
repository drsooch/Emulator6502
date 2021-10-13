-- | Error Types and handling
module Assembler.Error
    ( AssemblyError(..)
    , AnalysisError(..)
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

data AnalysisError = InvalidOpCodeAddressType AsmInstruction
                   | InvalidDirectiveSize LabeledLocation
                   | InvalidVariable Text AsmStatement
                   | DuplicateBinding Text
                   deriving Eq

data AssemblyError = AnalyzeError AnalysisError
                   | InternalError Text
                   | ParseError ParserError
                   deriving Eq

instance Show AssemblyError where
    show (AnalyzeError  aErr) = show aErr
    show (InternalError err ) = "Internal Error Occurred:\n\t" <> unpack err
    show (ParseError    pErr) = "Parsing Error Occurred:\n\t" <> errorBundlePretty pErr

instance Show AnalysisError where
    show (InvalidOpCodeAddressType cs) =
        "OpCode " <> show (opName cs) <> " contains invalid address type: " <> show (addressType cs)
    show (InvalidDirectiveSize labLoc) =
        "Directive contains invalid size:\n\t" <> show (directType labLoc)
    show (InvalidVariable _ _      ) = "Invalid Variable"
    show (DuplicateBinding bindName) = "Duplicate Binding: " <> unpack bindName

-- used for codepaths that shouldn't be reached to satisfy GHC
assertNotReached :: a
assertNotReached = assert False undefined
