module LoadStoreTest
    ( loadStore
    ) where

import           Control.Arrow                  ( (&&&) )
import           Control.Monad.State.Strict     ( (>=>)
                                                , gets
                                                , runStateT
                                                )
import           Data.Array.IArray              ( (//) )
import qualified Data.Array.IArray             as IA
import           Data.Bits                      ( Bits((.&.), (.|.)) )
import           Execution
import           Flags
import           Memory
import           System.IO                      ( hClose )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , testCase
                                                )
import           TestUtils                      ( mkTestCPU
                                                , runEmulatorTest
                                                )
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
    -> String                 -- ^ Test Name
    -> (String -> IO CPUState)
    -> TestTree
loadReg reg bytes expect flag name testCPU = testCase name $ do
    ((aReg, flags), cpu) <- testCPU name
        >>= runEmulatorTest (loadReg' reg bytes)
    hClose $ logLocation cpu
    Reg expect @=? aReg -- check A Register
    flag @=? flags .&. (zeroFlag .|. negFlag) -- Check Flag

loadReg' :: (CPUState -> Register) -> [Byte] -> Emulator (Register, Flags)
loadReg' reg bytes = setMemory 0xF000 bytes >> execute >> gets (reg &&& fReg)

{-------------------------------------- A Register Tests --------------------------------------}
loadAReg
    :: [Byte] -> Byte -> Flags -> String -> (String -> IO CPUState) -> TestTree
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
    (mkTestCPU >=> (\c -> return c { xReg = Reg 0x15 }))

loadARegZeroPageXWrap :: TestTree
loadARegZeroPageXWrap = loadAReg
    [0xB5, 0xD9]
    0xCF
    negFlag
    "Load A Register - ZeroPageX - Wrap"
    (mkTestCPU >=> (\c -> return c { xReg = Reg 0x57 }))

loadARegAbsolute :: TestTree
loadARegAbsolute = loadAReg
    [0xAD, 0x34, 0x12]
    0x57
    0x0
    "Load A Register - Absolute"
    (mkTestCPU >=> (\c -> return c { memory = memory c // [(0x1234, 0x57)] }))

loadARegAbsoluteX :: TestTree
loadARegAbsoluteX = loadAReg
    [0xBD, 0x34, 0x12]
    0x57
    0
    "Load A Register - AbsoluteX"
    (   mkTestCPU
    >=> (\c -> return c { xReg   = Reg 0xF4
                        , memory = memory c // [(0x1328, 0x57)]
                        }
        )
    )

loadARegAbsoluteY :: TestTree
loadARegAbsoluteY = loadAReg
    [0xB9, 0x34, 0x12]
    0x57
    0
    "Load A Register - AbsoluteY"
    (   mkTestCPU
    >=> (\c -> return c { yReg   = Reg 0xF4
                        , memory = memory c // [(0x1328, 0x57)]
                        }
        )
    )

loadARegIndirectX :: TestTree
loadARegIndirectX = loadAReg
    [0xA1, 0xC9]
    0xFF
    negFlag
    "Load A Register - IndirectX"
    (   mkTestCPU
    >=> (\c -> return c { xReg   = Reg 0xC0
                        , memory = memory c // [(0x7576, 0xFF)]
                        }
        )
    )

loadARegIndirectY :: TestTree
loadARegIndirectY = loadAReg
    [0xB1, 0xC9]
    0xFF
    negFlag
    "Load A Register - IndirectY"
    (   mkTestCPU
    >=> (\c -> return c { yReg   = Reg 0xBB
                        , memory = memory c // [(0x35F1, 0xFF)]
                        }
        )
    )

{-------------------------------------- X Register Tests --------------------------------------}
loadXReg
    :: [Byte] -> Byte -> Flags -> String -> (String -> IO CPUState) -> TestTree
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
    (mkTestCPU >=> (\c -> return c { yReg = 0x10 }))

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
    (   mkTestCPU
    >=> (\c -> return c { yReg   = Reg 0xAA
                        , memory = memory c // [(0x30AA, 0x21)]
                        }
        )
    )

{-------------------------------------- Y Register Tests --------------------------------------}
loadYReg
    :: [Byte] -> Byte -> Flags -> String -> (String -> IO CPUState) -> TestTree
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
    (mkTestCPU >=> (\c -> return c { xReg = Reg 0x10 }))

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
    (   mkTestCPU
    >=> (\c -> return c { xReg   = Reg 0xAA
                        , memory = memory c // [(0x30AA, 0x21)]
                        }
        )
    )
{-----------------------------------------------------------------------------------------}


{-------------------------------------- Store Tests --------------------------------------}

storeReg
    :: [Byte]            -- ^Program Instructions
    -> Address           -- ^Address to store value
    -> Byte              -- ^Expected Result in Memory
    -> String            -- ^Test Name
    -> (String -> IO CPUState)       -- ^CPU with setup data
    -> TestTree
storeReg bytes addr expect name testCPU = testCase name $ do
    cpu       <- testCPU name
    (recv, _) <- runStateT (storeReg' addr bytes) cpu
    hClose $ logLocation cpu
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
    (mkTestCPU >=> (\c -> return c { aReg = Reg 0x6A, xReg = 0x01 }))

storeARegAbsolute :: TestTree
storeARegAbsolute = storeReg
    [0x8D, 0xAA, 0x57]
    0x57AA
    0x4B
    "Store A Register - Absolute"
    (mkTestCPU >=> (\c -> return c { aReg = Reg 0x4B }))

{-------------------------------------- Store X Register --------------------------------------}
storeXRegZeroPage :: TestTree
storeXRegZeroPage = storeReg
    [0x86, 0xE1]
    0x00E1
    0x57
    "Store X Register - ZeroPage"
    (mkTestCPU >=> (\c -> return c { xReg = Reg 0x57 }))

storeXRegZeroPageY :: TestTree
storeXRegZeroPageY = storeReg
    [0x96, 0xE1]
    0x00E2
    0x6A
    "Store X Register - ZeroPageY"
    (mkTestCPU >=> (\c -> return c { xReg = Reg 0x6A, yReg = 0x01 }))

storeXRegAbsolute :: TestTree
storeXRegAbsolute = storeReg
    [0x8E, 0xAA, 0x57]
    0x57AA
    0x4B
    "Store X Register - Absolute"
    (mkTestCPU >=> (\c -> return c { xReg = Reg 0x4B }))

{-------------------------------------- Store Y Register --------------------------------------}
storeYRegZeroPage :: TestTree
storeYRegZeroPage = storeReg
    [0x84, 0xE1]
    0x00E1
    0x57
    "Store Y Register - ZeroPage"
    (mkTestCPU >=> (\c -> return c { yReg = Reg 0x57 }))

storeYRegZeroPageX :: TestTree
storeYRegZeroPageX = storeReg
    [0x94, 0xE1]
    0x00E2
    0x6A
    "Store Y Register - ZeroPageX"
    (mkTestCPU >=> (\c -> return c { yReg = Reg 0x6A, xReg = 0x01 }))

storeYRegAbsolute :: TestTree
storeYRegAbsolute = storeReg
    [0x8C, 0xAA, 0x57]
    0x57AA
    0x4B
    "Store Y Register - Absolute"
    (mkTestCPU >=> (\c -> return c { yReg = Reg 0x4B }))
