-- | Shift Operation Testing

module Test.ShiftTest
    ( shifts
    ) where


import           Data.Bits                      ( (.|.) )
import           Data.Functor                   ( (<&>) )
import           Execution
import           Flags
import           Register
import           Test.QuickCheck.Monadic        ( assert
                                                , monadicIO
                                                , pick
                                                , run
                                                )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , testCase
                                                )
import           Test.Tasty.QuickCheck
import           TestUtils
import           Types
import           Utils

shifts :: TestTree
shifts = testGroup "Shift Operations" [arithShiftLeft, logicShiftRight, rotateLeft, rotateRight]


{--------------------------------------  ASL Tests --------------------------------------}

arithShiftLeft :: TestTree
arithShiftLeft = testGroup
    "Arithmetic Shift Left Operations"
    [ arithShiftLeftAcc
    , arithShiftLeftZeroPage
    , arithShiftLeftZeroPageX
    , arithShiftLeftAbsolute
    , arithShiftLeftAbsoluteX
    , prop_ASLTasty
    ]

prop_ASLTasty :: TestTree
prop_ASLTasty = testProperty "ASL QuickCheck" prop_ASL

prop_ASL :: Property
prop_ASL = monadicIO $ do
    toShift <- pick arbitrary
    shifted <- run $ arithShiftLeftProp toShift
    assert (shifted == (toShift * 2))

arithShiftLeftProp :: Byte -> IO Byte
arithShiftLeftProp toShift = do
    cpu <- mkTestCPU "ASLQuickCheck" <&> setProgramMemory [0x0A] . setARegisterTest toShift
    (shiftVal, _) <- runEmulatorTest (execute >> getARegister) cpu
    return shiftVal

arithShiftLeftAcc :: TestTree
arithShiftLeftAcc = testCase "ASL - Accumulator" $ do
    cpu <- mkTestCPU "ASLAccumulator" <&> setProgramMemory [0x0A] . setARegisterTest toShift
    (shiftVal, cpu') <- runEmulatorTest (execute >> getARegister) cpu
    expected @=? shiftVal
    (carryFlag .|. negFlag) @=? fReg cpu'
  where
    toShift  = 0xF1
    expected = toShift * 2


arithShiftLeftZeroPage :: TestTree
arithShiftLeftZeroPage = testCase "ASL - ZeroPage" $ do
    cpu              <- mkTestCPU "ASLZeroPage" <&> setProgramMemory [0x06, toShiftZP]
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (fromIntegral toShiftZP :: Address))
        cpu
    expected @=? shiftVal
    negFlag @=? fReg cpu'
  where
    toShiftZP = 0xBF
    expected  = (0xFF - 0xBF) * 2

arithShiftLeftZeroPageX :: TestTree
arithShiftLeftZeroPageX = testCase "ASL - ZeroPage - X" $ do
    cpu <- mkTestCPU "ASLZeroPageX" <&> setProgramMemory [0x16, zpAddr] . setXRegisterTest xOffset
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (fromIntegral (zpAddr + xOffset) :: Address))
        cpu
    expected @=? shiftVal
    carryFlag @=? fReg cpu'
  where
    zpAddr   = 0x3F
    xOffset  = 0x14
    expected = (0xFF - (zpAddr + xOffset)) * 2

arithShiftLeftAbsolute :: TestTree
arithShiftLeftAbsolute = testCase "ASL - Absolute" $ do
    cpu <- mkTestCPU "ASLAbsolute" <&> setTestMemory shiftAddr [toShift] . setProgramMemory
        [0x0E, addrLsb, addrMsb]
    (shiftVal, cpu') <- runEmulatorTest (execute >> fetchByte shiftAddr) cpu
    expected @=? shiftVal
    (carryFlag .|. negFlag) @=? fReg cpu'
  where
    addrLsb   = 0x54
    addrMsb   = 0xAA
    toShift   = 0xFF
    shiftAddr = toAddress addrLsb addrMsb
    expected  = 0xFF * 2

arithShiftLeftAbsoluteX :: TestTree
arithShiftLeftAbsoluteX = testCase "ASL - AbsoluteX" $ do
    cpu <-
        mkTestCPU "ASLAbsoluteX"
        <&> setTestMemory (shiftAddr + fromIntegral xOffset) [toShift]
        .   setProgramMemory [0x1E, addrLsb, addrMsb]
        .   setXRegisterTest xOffset
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (shiftAddr + fromIntegral xOffset))
        cpu
    expected @=? shiftVal
    (carryFlag .|. negFlag) @=? fReg cpu'
  where
    addrLsb   = 0x54
    addrMsb   = 0xAA
    toShift   = 0xFF
    xOffset   = 0xF
    shiftAddr = toAddress addrLsb addrMsb
    expected  = 0xFF * 2

{--------------------------------------  LSR Tests --------------------------------------}

logicShiftRight :: TestTree
logicShiftRight = testGroup
    "Logical Shift Right"
    [ logicShiftRightAcc
    , logicShiftRightZeroPage
    , logicShiftRightZeroPageX
    , logicShiftRightAbsolute
    , logicShiftRightAbsoluteX
    , prop_LSRTasty
    ]

prop_LSRTasty :: TestTree
prop_LSRTasty = testProperty "LSR QuickCheck" prop_LSR

prop_LSR :: Property
prop_LSR = monadicIO $ do
    toShift <- pick arbitrary
    shifted <- run $ logicShiftRightProp toShift
    assert (shifted == (toShift `div` 2))

logicShiftRightProp :: Byte -> IO Byte
logicShiftRightProp toShift = do
    cpu <- mkTestCPU "LSRQuickCheck" <&> setProgramMemory [0x4A] . setARegisterTest toShift
    (shiftVal, _) <- runEmulatorTest (execute >> getARegister) cpu
    return shiftVal

logicShiftRightAcc :: TestTree
logicShiftRightAcc = testCase "LSR - Accumulator" $ do
    cpu <- mkTestCPU "LSRAccumulator" <&> setProgramMemory [0x4A] . setARegisterTest toShift
    (shiftVal, cpu') <- runEmulatorTest (execute >> getARegister) cpu
    expected @=? shiftVal
    carryFlag @=? fReg cpu'
  where
    toShift  = 0xF1
    expected = toShift `div` 2


logicShiftRightZeroPage :: TestTree
logicShiftRightZeroPage = testCase "LSR - ZeroPage" $ do
    cpu              <- mkTestCPU "LSRZeroPage" <&> setProgramMemory [0x46, toShiftZP]
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (fromIntegral toShiftZP :: Address))
        cpu
    expected @=? shiftVal
    0x0 @=? fReg cpu'
  where
    toShiftZP = 0xBF
    expected  = (0xFF - 0xBF) `div` 2

logicShiftRightZeroPageX :: TestTree
logicShiftRightZeroPageX = testCase "LSR - ZeroPage - X" $ do
    cpu <- mkTestCPU "LSRZeroPageX" <&> setProgramMemory [0x56, zpAddr] . setXRegisterTest xOffset
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (fromIntegral (zpAddr + xOffset) :: Address))
        cpu
    expected @=? shiftVal
    0x0 @=? fReg cpu'
  where
    zpAddr   = 0x3F
    xOffset  = 0x14
    expected = (0xFF - (zpAddr + xOffset)) `div` 2

logicShiftRightAbsolute :: TestTree
logicShiftRightAbsolute = testCase "LSR - Absolute" $ do
    cpu <- mkTestCPU "LSRAbsolute" <&> setTestMemory shiftAddr [toShift] . setProgramMemory
        [0x4E, addrLsb, addrMsb]
    (shiftVal, cpu') <- runEmulatorTest (execute >> fetchByte shiftAddr) cpu
    expected @=? shiftVal
    carryFlag @=? fReg cpu'
  where
    addrLsb   = 0x54
    addrMsb   = 0xAA
    toShift   = 0xFF
    shiftAddr = toAddress addrLsb addrMsb
    expected  = toShift `div` 2

logicShiftRightAbsoluteX :: TestTree
logicShiftRightAbsoluteX = testCase "LSR - AbsoluteX" $ do
    cpu <-
        mkTestCPU "LSRAbsoluteX"
        <&> setTestMemory (shiftAddr + fromIntegral xOffset) [toShift]
        .   setProgramMemory [0x5E, addrLsb, addrMsb]
        .   setXRegisterTest xOffset
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (shiftAddr + fromIntegral xOffset))
        cpu
    expected @=? shiftVal
    carryFlag @=? fReg cpu'
  where
    addrLsb   = 0x54
    addrMsb   = 0xAA
    toShift   = 0xFF
    xOffset   = 0xF
    shiftAddr = toAddress addrLsb addrMsb
    expected  = toShift `div` 2

{--------------------------------------  ROL Tests --------------------------------------}

rotateLeft :: TestTree
rotateLeft = testGroup
    "Rotate Left Operations"
    [ rotateLeftAcc
    , rotateLeftZeroPage
    , rotateLeftZeroPageX
    , rotateLeftAbsolute
    , rotateLeftAbsoluteX
    , prop_ROLTasty
    ]

prop_ROLTasty :: TestTree
prop_ROLTasty = testProperty "ROL QuickCheck" prop_ROL

prop_ROL :: Property
prop_ROL = monadicIO $ do
    toRotate <- pick arbitrary
    carry    <- pick $ elements [0, 1]
    shifted  <- run $ rotateLeftProp toRotate carry
    assert $ shifted == ((toRotate `shiftL` 1) + carry)

rotateLeftProp :: Byte -> Byte -> IO Byte
rotateLeftProp toRotate carry = do
    cpu <-
        mkTestCPU "ROLQuickCheck"
        <&> setProgramMemory [0x2A]
        .   setARegisterTest toRotate
        .   setFRegisterTest (if carry == 1 then carryFlag else 0x0)
    (shiftVal, _) <- runEmulatorTest (execute >> getARegister) cpu
    return shiftVal

rotateLeftAcc :: TestTree
rotateLeftAcc = testCase "ROL - Accumulator" $ do
    cpu <- mkTestCPU "ROLAccumulator" <&> setProgramMemory [0x2A] . setARegisterTest toRotate
    (shiftVal, cpu') <- runEmulatorTest (execute >> getARegister) cpu
    expected @=? shiftVal
    (carryFlag .|. negFlag) @=? fReg cpu'
  where
    toRotate = 0xF1
    expected = toRotate `shiftL` 1


rotateLeftZeroPage :: TestTree
rotateLeftZeroPage = testCase "ROL - ZeroPage" $ do
    cpu <- mkTestCPU "ROLZeroPage" <&> setProgramMemory [0x26, toRotateZP] . setFRegisterTest 0x1 -- add carry
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (fromIntegral toRotateZP :: Address))
        cpu
    expected @=? shiftVal
    negFlag @=? fReg cpu'
  where
    toRotateZP = 0xBF
    expected   = ((0xFF - 0xBF) `shiftL` 1) .|. 1

rotateLeftZeroPageX :: TestTree
rotateLeftZeroPageX = testCase "ROL - ZeroPage - X" $ do
    cpu <- mkTestCPU "ROLZeroPageX" <&> setProgramMemory [0x36, zpAddr] . setXRegisterTest xOffset
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (fromIntegral (zpAddr + xOffset) :: Address))
        cpu
    expected @=? shiftVal
    carryFlag @=? fReg cpu'
  where
    zpAddr   = 0x3F
    xOffset  = 0x14
    expected = (0xFF - (zpAddr + xOffset)) `shiftL` 1

rotateLeftAbsolute :: TestTree
rotateLeftAbsolute = testCase "ROL - Absolute" $ do
    cpu <-
        mkTestCPU "ROLAbsolute"
        <&> setTestMemory shiftAddr [toRotate]
        .   setProgramMemory [0x2E, addrLsb, addrMsb]
        .   setFRegisterTest 0x1
    (shiftVal, cpu') <- runEmulatorTest (execute >> fetchByte shiftAddr) cpu
    expected @=? shiftVal
    (carryFlag .|. negFlag) @=? fReg cpu'
  where
    addrLsb   = 0x54
    addrMsb   = 0xAA
    toRotate  = 0xFF
    shiftAddr = toAddress addrLsb addrMsb
    expected  = (toRotate `shiftL` 1) .|. 1

rotateLeftAbsoluteX :: TestTree
rotateLeftAbsoluteX = testCase "ROL - AbsoluteX" $ do
    cpu <-
        mkTestCPU "ROLAbsoluteX"
        <&> setTestMemory (shiftAddr + fromIntegral xOffset) [toRotate]
        .   setProgramMemory [0x3E, addrLsb, addrMsb]
        .   setXRegisterTest xOffset
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (shiftAddr + fromIntegral xOffset))
        cpu
    expected @=? shiftVal
    (carryFlag .|. negFlag) @=? fReg cpu'
  where
    addrLsb   = 0x54
    addrMsb   = 0xAA
    toRotate  = 0xFF
    xOffset   = 0xF
    shiftAddr = toAddress addrLsb addrMsb
    expected  = toRotate `shiftL` 1

{--------------------------------------  ROR Tests --------------------------------------}

rotateRight :: TestTree
rotateRight = testGroup
    "Rotate Right Operations"
    [ rotateRightAcc
    , rotateRightZeroPage
    , rotateRightZeroPageX
    , rotateRightAbsolute
    , rotateRightAbsoluteX
    , prop_RORTasty
    ]

prop_RORTasty :: TestTree
prop_RORTasty = testProperty "ROR QuickCheck" prop_ROR

prop_ROR :: Property
prop_ROR = monadicIO $ do
    toRotate <- pick arbitrary
    carry    <- pick $ elements [0, 0x80]
    shifted  <- run $ rotateRightProp toRotate carry
    assert $ shifted == ((toRotate `shiftR` 1) + carry)

rotateRightProp :: Byte -> Byte -> IO Byte
rotateRightProp toRotate carry = do
    cpu <-
        mkTestCPU "RORQuickCheck"
        <&> setProgramMemory [0x6A]
        .   setARegisterTest toRotate
        .   setFRegisterTest (if carry == 0x80 then carryFlag else 0x0)
    (shiftVal, _) <- runEmulatorTest (execute >> getARegister) cpu
    return shiftVal

rotateRightAcc :: TestTree
rotateRightAcc = testCase "ROR - Accumulator" $ do
    cpu <-
        mkTestCPU "RORAccumulator"
        <&> setProgramMemory [0x6A]
        .   setARegisterTest toRotate
        .   setFRegisterTest 0x1
    (shiftVal, cpu') <- runEmulatorTest (execute >> getARegister) cpu
    expected @=? shiftVal
    (carryFlag .|. negFlag) @=? fReg cpu'
  where
    toRotate = 0xF1
    expected = (toRotate `shiftR` 1) .|. 0x80


rotateRightZeroPage :: TestTree
rotateRightZeroPage = testCase "ROR - ZeroPage" $ do
    cpu              <- mkTestCPU "RORZeroPage" <&> setProgramMemory [0x66, toRotateZP]
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (fromIntegral toRotateZP :: Address))
        cpu
    expected @=? shiftVal
    0x0 @=? fReg cpu'
  where
    toRotateZP = 0xBF
    expected   = (0xFF - 0xBF) `shiftR` 1

rotateRightZeroPageX :: TestTree
rotateRightZeroPageX = testCase "ROR - ZeroPage - X" $ do
    cpu <- mkTestCPU "RORZeroPageX" <&> setProgramMemory [0x76, zpAddr] . setXRegisterTest xOffset
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (fromIntegral (zpAddr + xOffset) :: Address))
        cpu
    expected @=? shiftVal
    0x0 @=? fReg cpu'
  where
    zpAddr   = 0x3F
    xOffset  = 0x14
    expected = (0xFF - (zpAddr + xOffset)) `shiftR` 1

rotateRightAbsolute :: TestTree
rotateRightAbsolute = testCase "ROR - Absolute" $ do
    cpu <- mkTestCPU "RORAbsolute" <&> setTestMemory shiftAddr [toRotate] . setProgramMemory
        [0x6E, addrLsb, addrMsb]
    (shiftVal, cpu') <- runEmulatorTest (execute >> fetchByte shiftAddr) cpu
    expected @=? shiftVal
    carryFlag @=? fReg cpu'
  where
    addrLsb   = 0x54
    addrMsb   = 0xAA
    toRotate  = 0xFF
    shiftAddr = toAddress addrLsb addrMsb
    expected  = toRotate `shiftR` 1

rotateRightAbsoluteX :: TestTree
rotateRightAbsoluteX = testCase "ROR - AbsoluteX" $ do
    cpu <-
        mkTestCPU "RORAbsoluteX"
        <&> setTestMemory (shiftAddr + fromIntegral xOffset) [toRotate]
        .   setProgramMemory [0x7E, addrLsb, addrMsb]
        .   setXRegisterTest xOffset
    (shiftVal, cpu') <- runEmulatorTest
        (execute >> fetchByte (shiftAddr + fromIntegral xOffset))
        cpu
    expected @=? shiftVal
    carryFlag @=? fReg cpu'
  where
    addrLsb   = 0x54
    addrMsb   = 0xAA
    toRotate  = 0xFF
    xOffset   = 0xF
    shiftAddr = toAddress addrLsb addrMsb
    expected  = toRotate `shiftR` 1
