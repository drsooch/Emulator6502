-- | Analyze a AsmTree

{-# LANGUAGE TupleSections #-}
module Assembler.Analyze
    ( analyzeStatements
    , validateLabeledLoc
    ) where

import           Assembler.Error
import           Assembler.Types
import           Assembler.Utils
import           Control.Monad                  ( foldM )
import           Control.Monad.Except           ( Except
                                                , runExcept
                                                , throwError
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , local
                                                , runReaderT
                                                )
import           Data.Foldable                  ( foldlM )
import           Data.List                      ( intersect )
import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Types

type BindingsEnv a = ReaderT (Set Text) (Except AssemblyError) a

analyzeStatements :: AsmTree -> Either AssemblyError Bindings
analyzeStatements tree = runExcept (runReaderT (analyzeStatements' tree) S.empty)

-- Scrape definitions, code blocks and labeled memory for all possible Labels
analyzeStatements' :: AsmTree -> BindingsEnv Bindings
analyzeStatements' AsmTree {..} = do
    defs      <- captureDefines definitions
    cLabels   <- local (toSet defs <>) (captureLabels codeBlocks)
    locLabels <- local ((toSet defs <> toSet cLabels) <>) (captureLabeledLoc labeledLocs)
    pure $ Bindings defs cLabels locLabels
    where toSet = S.fromList . M.keys

-- Check the global environment for duplicate labels
checkGlobalLabels :: Text -> c -> Map Text c -> BindingsEnv (Map Text c)
checkGlobalLabels label val vars = ask >>= \defined -> if label `S.member` defined
    then throwError $ DuplicateLabel label
    else pure $ M.insert label val vars

-- capture the defining statements
captureDefines :: [VarDefinition] -> BindingsEnv Variables
captureDefines = foldlM capture M.empty
  where
    capture :: Variables -> VarDefinition -> BindingsEnv Variables
    capture vars VarDefinition {..}
        | varName `M.member` vars = throwError $ DuplicateVariable varName
        | otherwise               = checkGlobalLabels varName value vars

-- capture any code labels and check to make sure we have valid instructions
-- label resolution occurs in CodeGen
captureLabels :: [CodeBlock] -> BindingsEnv CodeLabels
captureLabels = foldlM capture M.empty
  where
    capture :: CodeLabels -> CodeBlock -> BindingsEnv CodeLabels
    capture codeLabels CodeBlock {..}
        | blockLabel `M.member` codeLabels
        = throwError $ DuplicateLabel blockLabel
        | otherwise
        = foldlM validateStatement () statements >> checkGlobalLabels blockLabel 0 codeLabels

-- construct a table of sizes/offsets of labeled locations
captureLabeledLoc :: [LabeledLocation] -> BindingsEnv MemLocations
captureLabeledLoc stmts =
    fst <$> foldM (\acc stmt -> validateLabeledLoc acc stmt >> capture acc stmt) (M.empty, 0) stmts
  where
    capture :: (MemLocations, Int) -> LabeledLocation -> BindingsEnv (MemLocations, Int)
    capture (labeledLocs, offset) LabeledLoc {..}
        | locLabel `M.member` labeledLocs
        = throwError $ DuplicateLabel locLabel
        | otherwise
        = let size = sizeOfLocation directType
          in  (, size) <$> checkGlobalLabels locLabel (offset, size) labeledLocs

sizeOfLocation :: AsmDirectiveType -> Int
sizeOfLocation = \case
    (DtByte  bytes ) -> length bytes
    (DtDByte dbytes) -> length dbytes
    (DtWord  word  ) -> length word
    (DtBlock size  ) -> fromIntegral $ getAsmNumericVal size
    (DtText  str   ) -> T.length str
    DtEnd            -> 0

-- validate opcodes
validateStatement :: a -> CodeStatement -> BindingsEnv a
validateStatement acc op@CodeStatement {..} | isValidAddressType opName addressType = pure acc
                                            | otherwise = throwError $ InvalidOpCodeAddressType op

-- validate labeled location size
validateLabeledLoc :: a -> LabeledLocation -> BindingsEnv a
validateLabeledLoc acc loc@LabeledLoc {..}
    | directType == DtEnd || sizeOfLocation directType > 0 = pure acc
    | otherwise = throwError $ InvalidDirectiveSize loc

-- converts StmtOpCode AsmAddressType to it's associated AddressType
-- and then checks to see if the AddressType is allowed
isValidAddressType :: OpName -> AsmAddressType -> Bool
isValidAddressType opn = not . null . intersect (validOperations opn) . convertAddressType

convertAddressType :: AsmAddressType -> [AddressType]
convertAddressType addrT = case addrT of
    (AsmImmediate _           ) -> [Immediate]
    (AsmZeroPage _ (Just AsmX)) -> [ZeroPageX]
    (AsmZeroPage _ (Just AsmY)) -> [ZeroPageY]
    (AsmZeroPage _ _          ) -> [ZeroPage]
    (AsmAbsolute _ (Just AsmX)) -> [AbsoluteX]
    (AsmAbsolute _ (Just AsmY)) -> [AbsoluteY]
    (AsmAbsolute _ _          ) -> [Absolute]
    (AsmIndirect _ (Just AsmX)) -> [IndirectX]
    (AsmIndirect _ (Just AsmY)) -> [IndirectY]
    (AsmIndirect _ _          ) -> [Indirect]
    (AsmRelative _            ) -> [Relative]
    AsmAccumulator              -> [Accumulator]
    AsmImplicit                 -> [Implicit]
    (AsmUnknown    _)           -> [Absolute, ZeroPage]
    (AsmUnknownImm _)           -> [Immediate]

validOperations :: OpName -> [AddressType]
validOperations = \case
    LDA -> [Immediate, ZeroPage, ZeroPageX, Absolute, AbsoluteX, AbsoluteY, IndirectX, IndirectY]
    LDX -> [Immediate, ZeroPage, ZeroPageY, Absolute, AbsoluteY]
    LDY -> [Immediate, ZeroPage, ZeroPageX, Absolute, AbsoluteX]
    STA -> [ZeroPage, ZeroPageX, Absolute, AbsoluteX, AbsoluteY, IndirectX, IndirectY]
    STX -> [ZeroPage, ZeroPageY, Absolute]
    STY -> [ZeroPage, ZeroPageX, Absolute]
    TAX -> [Implicit]
    TAY -> [Implicit]
    TXA -> [Implicit]
    TYA -> [Implicit]
    TSX -> [Implicit]
    TXS -> [Implicit]
    PHP -> [Implicit]
    PHA -> [Implicit]
    PLA -> [Implicit]
    PLP -> [Implicit]
    AND -> [Immediate, ZeroPage, ZeroPageX, Absolute, AbsoluteX, AbsoluteY, IndirectX, IndirectY]
    EOR -> [Immediate, ZeroPage, ZeroPageX, Absolute, AbsoluteX, AbsoluteY, IndirectX, IndirectY]
    ORA -> [Immediate, ZeroPage, ZeroPageX, Absolute, AbsoluteX, AbsoluteY, IndirectX, IndirectY]
    BIT -> [ZeroPage, Absolute]
    ADC -> [Immediate, ZeroPage, ZeroPageX, Absolute, AbsoluteX, AbsoluteY, IndirectX, IndirectY]
    SBC -> [Immediate, ZeroPage, ZeroPageX, Absolute, AbsoluteX, AbsoluteY, IndirectX, IndirectY]
    CMP -> [Immediate, ZeroPage, ZeroPageX, Absolute, AbsoluteX, AbsoluteY, IndirectX, IndirectY]
    CPX -> [Immediate, ZeroPage, Absolute]
    CPY -> [Immediate, ZeroPage, Absolute]
    INC -> [Implicit]
    INX -> [Implicit]
    INY -> [Implicit]
    DEC -> [Implicit]
    DEX -> [Implicit]
    DEY -> [Implicit]
    ASL -> [Accumulator, ZeroPage, ZeroPageX, Absolute, AbsoluteX]
    LSR -> [Accumulator, ZeroPage, ZeroPageX, Absolute, AbsoluteX]
    ROL -> [Accumulator, ZeroPage, ZeroPageX, Absolute, AbsoluteX]
    ROR -> [Accumulator, ZeroPage, ZeroPageX, Absolute, AbsoluteX]
    JMP -> [Absolute, Indirect]
    JSR -> [Absolute]
    RTS -> [Implicit]
    BCC -> [Relative]
    BCS -> [Relative]
    BEQ -> [Relative]
    BMI -> [Relative]
    BNE -> [Relative]
    BPL -> [Relative]
    BVC -> [Relative]
    BVS -> [Relative]
    CLC -> [Implicit]
    CLD -> [Implicit]
    CLI -> [Implicit]
    CLV -> [Implicit]
    SEC -> [Implicit]
    SED -> [Implicit]
    SEI -> [Implicit]
    NOP -> [Implicit]
    BRK -> [Implicit]
    RTI -> [Implicit]
