-- | Testing ADC with additional OVERFLOW and CARRY tests

module ADCTest
    ( adcCarry
    , adder
    , adcOverflow
    ) where

import           Control.Monad.RWS.Strict       ( )
import           Data.Foldable                  ( foldl' )
import           Data.Word                      ( Word16 )
import           Flags
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( assertBool
                                                , testCase
                                                )
import           Types

adder :: TestTree
adder = testGroup "ADC Operations" []

adderTriplets :: [(Byte, Byte, Byte)]
adderTriplets =
    [ (acc, addend, carry)
    | acc    <- [0x0 .. 0xFF]
    , addend <- [0x0 .. 0xFF]
    , carry  <- [0x0, 0x1]
    ]

{-------------------------------------- ADC Overflow Test --------------------------------------}

adcOverflow :: TestTree
adcOverflow = testCase "Overflow Testing for ADC" $ do
    undefined

{-------------------------------------- ADC Carry Test    --------------------------------------}

-- Don't want to litter the entire test suite with single cases
-- So we just assert everything passes
adcCarry :: TestTree
adcCarry = testCase "Carry Testing for ADC" $ do
    assertBool "Checking Carry bits" $ foldl'
        (\b (acc, add, car) -> b && addAndCheckCF acc add car)
        True
        adderTriplets

addAndCheckCF :: Byte -> Byte -> Byte -> Bool
addAndCheckCF acc addend carry =
    let result =
            (fromIntegral acc :: Word16)
                + (fromIntegral addend :: Word16)
                + (fromIntegral carry :: Word16)
        -- Since we only deal with bytes we will lose any upper bits
        -- The conversion here is what isCarryAdd could do
        -- but the current implementation is much cleaner
    in  if isCarryAdd acc (acc + addend + carry) carry
            then result > 255
            else result <= 255
