-- | Utility Functions for the assembler

module Assembler.Utils
    ( escapeChars
    ) where

escapeChars :: String -> String
escapeChars = init . tail . show
