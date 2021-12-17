-- | Testing ADC with additional OVERFLOW and CARRY tests

module Test.SBCTest
    ( sbcCarry
    , subber
    , sbcOverflow
    ) where

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

subber :: TestTree
subber = testGroup "ADC Operations" []

subberTriplets :: [(Byte, Byte, Byte)]
subberTriplets =
    [ (acc, addend, carry)
    | acc    <- [0x0 .. 0xFF]
    , addend <- [0x0 .. 0xFF]
    , carry  <- [0x0, 0x1]
    ]

{-------------------------------------- ADC Overflow Test --------------------------------------}

sbcOverflow :: TestTree
sbcOverflow = testCase "Overflow Testing for SBC" $ do
    undefined

{-------------------------------------- ADC Carry Test    --------------------------------------}

-- Don't want to litter the entire test suite with single cases
-- So we just assert everything passes
sbcCarry :: TestTree
sbcCarry = testCase "Carry Testing for SBC" $ do
    assertBool "Checking Carry bits" $ foldl'
        (\b (acc, sub, car) -> b && subAndCheckCF acc sub car)
        True
        subberTriplets

-- FIXME: Is this even necessary
subAndCheckCF :: Byte -> Byte -> Byte -> Bool
subAndCheckCF acc subbend carry =
    let carry' = if carry == 1 then 0 else 1 :: Word16
        result =
            (fromIntegral acc :: Word16)
                - (fromIntegral subbend :: Word16)
                - carry'
        -- Since we only deal with bytes we will lose any upper bits
        -- The conversion here is what isCarryAdd could do
        -- but the current implementation is much cleaner
    in  if isCarrySub acc subbend then result > 255 else result <= 255
