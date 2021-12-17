module Test.LoadStoreTest
    ( loadStore
    ) where

import           Control.Arrow                  ( (&&&) )
import           Control.Monad.State.Strict     ( (>=>)
                                                , gets
                                                )
import qualified Data.Array.IArray             as IA
import           Data.Bits                      ( Bits((.&.), (.|.)) )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Execution
import           Flags
import           Memory
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , testCase
                                                )
import           TestUtils
import           Types

loadStore :: TestTree
loadStore = testGroup
    "LoadStore Operations"
    [ loadARegTestGroup
    , loadXRegTestGroup
    , loadYRegTestGroup
    , storeARegTestGroup
    , storeXRegTestGroup
    , storeYRegTestGroup
    ]

{-------------------------------------- TestGroups --------------------------------------}
loadYRegTestGroup :: TestTree
loadYRegTestGroup =
    testGroup "Loading Y Register"
        $  [loadYRegImmediate, loadYRegZeroPage, loadYRegZeroPageX]
        <> [loadYRegAbsolute, loadYRegAbsoluteX]

loadXRegTestGroup :: TestTree
loadXRegTestGroup =
    testGroup "Loading X Register"
        $  [loadXRegImmediate, loadXRegZeroPage, loadXRegZeroPageY]
        <> [loadXRegAbsolute, loadXRegAbsoluteY]

loadARegTestGroup :: TestTree
loadARegTestGroup =
    testGroup "Loading A Register"
        $  [loadARegImmediatePos, loadARegImmediateNeg, loadARegImmediateZero]
        <> [loadARegZeroPage, loadARegZeroPageXNoWrap, loadARegZeroPageXWrap]
        <> [loadARegAbsolute, loadARegAbsoluteX, loadARegAbsoluteY]
        <> [loadARegIndirectX, loadARegIndirectY]

storeARegTestGroup :: TestTree
storeARegTestGroup = testGroup
    "Storing A Register"
    [storeARegZeroPage, storeARegZeroPageX, storeARegAbsolute]

storeXRegTestGroup :: TestTree
storeXRegTestGroup = testGroup
    "Storing X Register"
    [storeXRegZeroPage, storeXRegZeroPageY, storeXRegAbsolute]

storeYRegTestGroup :: TestTree
storeYRegTestGroup = testGroup
    "Storing Y Register"
    [storeYRegZeroPage, storeYRegZeroPageX, storeYRegAbsolute]

-- Execute a single Load instruction and check for proper output
loadReg
    :: (CPUState -> Register) -- ^ Which register we are testing
    -> [Byte]                 -- ^ Bytes to insert into memory
    -> Byte                   -- ^ Expected Result in Register
    -> Flags                  -- ^ Expected Flag Bits
    -> Text                 -- ^ Test Name
    -> (Text -> IO CPUState)
    -> TestTree
loadReg reg bytes expect flag name testCPU = testCase (unpack name) $ do
    ((aReg, flags), _) <- testCPU name >>= runEmulatorTest (loadReg' reg bytes)
    Reg expect @=? aReg -- check A Register
    flag @=? flags .&. (zeroFlag .|. negFlag) -- Check Flag

loadReg' :: (CPUState -> Register) -> [Byte] -> Emulator (Register, Flags)
loadReg' reg bytes = setMemory 0xF000 bytes >> execute >> gets (reg &&& fReg)

{-------------------------------------- A Register Tests --------------------------------------}
loadAReg
    :: [Byte] -> Byte -> Flags -> Text -> (Text -> IO CPUState) -> TestTree
loadAReg = loadReg aReg

-- NOTE: We test the flag bits explicitly using LDA Immediate
-- The rest of the addressing modes only deal with special cases (Page boundary) not flags
loadARegImmediatePos :: TestTree
loadARegImmediatePos = loadAReg [0xA9, 0x39]
                                0x39
                                0x0
                                "Load A Register - Immediate - Positive"
                                mkTestCPU

loadARegImmediateNeg :: TestTree
loadARegImmediateNeg = loadAReg [0xA9, 0x9D]
                                0x9D
                                negFlag
                                "Load A Register - Immediate - Negative"
                                mkTestCPU

loadARegImmediateZero :: TestTree
loadARegImmediateZero = loadAReg [0xA9, 0x0]
                                 0x0
                                 zeroFlag
                                 "Load A Register - Immediate - Zero"
                                 mkTestCPU

loadARegZeroPage :: TestTree
loadARegZeroPage =
    loadAReg [0xA5, 0x27] 0xD8 negFlag "Load A Register - ZeroPage" mkTestCPU

loadARegZeroPageXNoWrap :: TestTree
loadARegZeroPageXNoWrap = loadAReg
    [0xB5, 0x34]
    0xB6
    negFlag
    "Load A Register - ZeroPageX - No Wrap"
    (mkTestCPU >=> \c -> return (setXRegisterTest 0x15 c))

loadARegZeroPageXWrap :: TestTree
loadARegZeroPageXWrap = loadAReg
    [0xB5, 0xD9]
    0xCF
    negFlag
    "Load A Register - ZeroPageX - Wrap"
    (mkTestCPU >=> \c -> return (setXRegisterTest 0x57 c))

loadARegAbsolute :: TestTree
loadARegAbsolute = loadAReg
    [0xAD, 0x34, 0x12]
    0x57
    0x0
    "Load A Register - Absolute"
    (mkTestCPU >=> \c -> return (setTestMemory 0x1234 [0x57] c))

loadARegAbsoluteX :: TestTree
loadARegAbsoluteX = loadAReg
    [0xBD, 0x34, 0x12]
    0x57
    0
    "Load A Register - AbsoluteX"
    (mkTestCPU >=> \c ->
        return (setTestMemory 0x1328 [0x57] (setXRegisterTest 0xF4 c))
    )

loadARegAbsoluteY :: TestTree
loadARegAbsoluteY = loadAReg
    [0xB9, 0x34, 0x12]
    0x57
    0
    "Load A Register - AbsoluteY"
    (mkTestCPU >=> \c ->
        return (setTestMemory 0x1328 [0x57] (setYRegisterTest 0xF4 c))
    )

loadARegIndirectX :: TestTree
loadARegIndirectX = loadAReg
    [0xA1, 0xC9]
    0xFF
    negFlag
    "Load A Register - IndirectX"
    (mkTestCPU >=> \c ->
        return (setTestMemory 0x7576 [0xFF] (setXRegisterTest 0xC0 c))
    )

loadARegIndirectY :: TestTree
loadARegIndirectY = loadAReg
    [0xB1, 0xC9]
    0xFF
    negFlag
    "Load A Register - IndirectY"
    (mkTestCPU >=> \c ->
        return (setTestMemory 0x35F1 [0xFF] (setYRegisterTest 0xBB c))
    )

{-------------------------------------- X Register Tests --------------------------------------}
loadXReg
    :: [Byte] -> Byte -> Flags -> Text -> (Text -> IO CPUState) -> TestTree
loadXReg = loadReg xReg

loadXRegImmediate :: TestTree
loadXRegImmediate =
    loadXReg [0xA2, 0x39] 0x39 0x0 "Load X Register - Immediate" mkTestCPU

loadXRegZeroPage :: TestTree
loadXRegZeroPage =
    loadXReg [0xA6, 0x0] 0xFF negFlag "Load X Register - ZeroPage" mkTestCPU

loadXRegZeroPageY :: TestTree
loadXRegZeroPageY = loadXReg
    [0xB6, 0x7F]
    0x70
    0x0
    "Load X Register - ZeroPageY"
    (mkTestCPU >=> \c -> return (setYRegisterTest 0x10 c))

loadXRegAbsolute :: TestTree
loadXRegAbsolute = loadXReg [0xAE, 0x00, 0xF0]
                            0xAE
                            negFlag
                            "Load X Register - Absolute"
                            mkTestCPU

loadXRegAbsoluteY :: TestTree
loadXRegAbsoluteY = loadXReg
    [0xBE, 0x00, 0x30]
    0x21
    0x0
    "Load X Register - AbsoluteY"
    (mkTestCPU >=> \c ->
        return (setTestMemory 0x30AA [0x21] (setYRegisterTest 0xAA c))
    )



{-------------------------------------- Y Register Tests --------------------------------------}
loadYReg
    :: [Byte] -> Byte -> Flags -> Text -> (Text -> IO CPUState) -> TestTree
loadYReg = loadReg yReg

loadYRegImmediate :: TestTree
loadYRegImmediate =
    loadYReg [0xA0, 0x41] 0x41 0x0 "Load Y Register - Immediate" mkTestCPU

loadYRegZeroPage :: TestTree
loadYRegZeroPage =
    loadYReg [0xA4, 0x0] 0xFF negFlag "Load Y Register - ZeroPage" mkTestCPU

loadYRegZeroPageX :: TestTree
loadYRegZeroPageX = loadYReg
    [0xB4, 0x7F]
    0x70
    0x0
    "Load Y Register - ZeroPageX"
    (mkTestCPU >=> \c -> return (setXRegisterTest 0x10 c))

loadYRegAbsolute :: TestTree
loadYRegAbsolute = loadYReg [0xAC, 0x00, 0xF0]
                            0xAC
                            negFlag
                            "Load Y Register - Absolute"
                            mkTestCPU

loadYRegAbsoluteX :: TestTree
loadYRegAbsoluteX = loadYReg
    [0xBC, 0x00, 0x30]
    0x21
    0x0
    "Load Y Register - AbsoluteX"
    (mkTestCPU >=> \c ->
        return (setTestMemory 0x30AA [0x21] (setXRegisterTest 0xAA c))
    )

{-----------------------------------------------------------------------------------------}


{-------------------------------------- Store Tests --------------------------------------}

storeReg
    :: [Byte]            -- ^Program Instructions
    -> Address           -- ^Address to store value
    -> Byte              -- ^Expected Result in Memory
    -> Text            -- ^Test Name
    -> (Text -> IO CPUState)       -- ^CPU with setup data
    -> TestTree
storeReg bytes addr expect name testCPU = testCase (unpack name) $ do
    cpu       <- testCPU name
    (recv, _) <- runEmulatorTest (storeReg' addr bytes) cpu
    expect @=? recv  -- Check Memory Loc for saved Register Value

storeReg' :: Address -> [Byte] -> Emulator Byte
storeReg' addr bytes =
    setMemory 0xF000 bytes >> execute >> gets (\c -> memory c IA.! addr)

{-------------------------------------- Store A Register --------------------------------------}
storeARegZeroPage :: TestTree
storeARegZeroPage = storeReg
    [0x85, 0xE1]
    0x00E1
    0x57
    "Store A Register - ZeroPage"
    (mkTestCPU >=> (\c -> return c { aReg = Reg 0x57 }))

storeARegZeroPageX :: TestTree
storeARegZeroPageX = storeReg
    [0x95, 0xE1]
    0x00E2
    0x6A
    "Store A Register - ZeroPageX"
    (   mkTestCPU
    >=> \c -> return (setARegisterTest 0x6A (setXRegisterTest 0x01 c))
    )


storeARegAbsolute :: TestTree
storeARegAbsolute = storeReg
    [0x8D, 0xAA, 0x57]
    0x57AA
    0x4B
    "Store A Register - Absolute"
    (mkTestCPU >=> \c -> return (setARegisterTest 0x4B c))

{-------------------------------------- Store X Register --------------------------------------}
storeXRegZeroPage :: TestTree
storeXRegZeroPage = storeReg
    [0x86, 0xE1]
    0x00E1
    0x57
    "Store X Register - ZeroPage"
    (mkTestCPU >=> \c -> return (setXRegisterTest 0x57 c))

storeXRegZeroPageY :: TestTree
storeXRegZeroPageY = storeReg
    [0x96, 0xE1]
    0x00E2
    0x6A
    "Store X Register - ZeroPageY"
    (   mkTestCPU
    >=> \c -> return (setXRegisterTest 0x6A (setYRegisterTest 0x01 c))
    )


storeXRegAbsolute :: TestTree
storeXRegAbsolute = storeReg
    [0x8E, 0xAA, 0x57]
    0x57AA
    0x4B
    "Store X Register - Absolute"
    (mkTestCPU >=> \c -> return (setXRegisterTest 0x4B c))

{-------------------------------------- Store Y Register --------------------------------------}
storeYRegZeroPage :: TestTree
storeYRegZeroPage = storeReg
    [0x84, 0xE1]
    0x00E1
    0x57
    "Store Y Register - ZeroPage"
    (mkTestCPU >=> \c -> return (setYRegisterTest 0x57 c))

storeYRegZeroPageX :: TestTree
storeYRegZeroPageX = storeReg
    [0x94, 0xE1]
    0x00E2
    0x6A
    "Store Y Register - ZeroPageX"
    (   mkTestCPU
    >=> \c -> return (setYRegisterTest 0x6A (setXRegisterTest 0x01 c))
    )

storeYRegAbsolute :: TestTree
storeYRegAbsolute = storeReg
    [0x8C, 0xAA, 0x57]
    0x57AA
    0x4B
    "Store Y Register - Absolute"
    (mkTestCPU >=> \c -> return (setYRegisterTest 0x4B c))
