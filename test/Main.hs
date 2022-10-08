-- Derived from @primitive@ package.

{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad (when, unless)
import Data.Array.Byte (ByteArray)
import Data.Function (on)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..), stimesMonoid)
import Data.Word (Word8)
import GHC.Exts (IsList(..))
import Language.Haskell.TH.Syntax (lift)
import Test.QuickCheck.Classes.Base (eqLaws, ordLaws, monoidLaws, showLaws, isListLaws, semigroupLaws, semigroupMonoidLaws, Laws(..))
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), (===), Property, NonNegative(..), property)

main :: IO ()
main = do
  testByteArray
  defaultMain $ testGroup "ByteArray"
    [ testProperty "equality" byteArrayEqProp
    , testProperty "reflexivity" byteArrayReflexivityProp
    , testProperty "compare" byteArrayCompareProp
    , lawsToTest (eqLaws (Proxy :: Proxy ByteArray))
    , lawsToTest (ordLaws (Proxy :: Proxy ByteArray))
    , lawsToTest (monoidLaws (Proxy :: Proxy ByteArray))
    , lawsToTest (showLaws (Proxy :: Proxy ByteArray))
    , lawsToTest (isListLaws (Proxy :: Proxy ByteArray))
    , lawsToTest (semigroupLaws (Proxy :: Proxy ByteArray))
    , lawsToTest (semigroupMonoidLaws (Proxy :: Proxy ByteArray))
    , testProperty "stimes" byteArrayStimesProp
    , testProperty "lift" byteArrayLiftProp
    ]

byteArrayCompareProp :: [Word8] -> [Word8] -> Property
byteArrayCompareProp xs ys =
  compareLengthFirst xs ys === compare (mkByteArray xs) (mkByteArray ys)

byteArrayEqProp :: [Word8] -> [Word8] -> Property
byteArrayEqProp xs ys =
  (compareLengthFirst xs ys == EQ) === (mkByteArray xs == mkByteArray ys)

byteArrayReflexivityProp :: [Word8] -> Property
byteArrayReflexivityProp xs = property $
  let ba = mkByteArray xs in ba == ba && compare ba ba == EQ

compareLengthFirst :: [Word8] -> [Word8] -> Ordering
compareLengthFirst xs ys = (compare `on` length) xs ys <> compare xs ys

byteArrayStimesProp :: NonNegative Int -> ByteArray -> Property
byteArrayStimesProp (NonNegative n) xs = stimes n xs === stimesMonoid n xs

byteArrayLiftProp :: Property
byteArrayLiftProp =
  let ba = mkByteArray [0,1,2,3,4,5] in ba === $(lift (fromList [0,1,2,3,4,5] :: ByteArray))

lawsToTest :: Laws -> TestTree
lawsToTest (Laws name pairs) = testGroup name (map (uncurry testProperty) pairs)

testByteArray :: IO ()
testByteArray = do
    let arr1 = mkByteArray ([0xde, 0xad, 0xbe, 0xef] :: [Word8])
        arr2 = mkByteArray ([0xde, 0xad, 0xbe, 0xef] :: [Word8])
        arr3 = mkByteArray ([0xde, 0xad, 0xbe, 0xee] :: [Word8])
        arr4 = mkByteArray ([0xde, 0xad, 0xbe, 0xdd] :: [Word8])
        arr5 = mkByteArray ([0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xdd] :: [Word8])
        arr6 = mkByteArray ([0xde, 0xad, 0x00, 0x01, 0xb0] :: [Word8])
    when (show arr1 /= "[0xde, 0xad, 0xbe, 0xef]") $
        fail $ "ByteArray Show incorrect: " ++ show arr1
    when (show arr6 /= "[0xde, 0xad, 0x00, 0x01, 0xb0]") $
        fail $ "ByteArray Show incorrect: " ++ show arr6
    unless (arr1 > arr3) $
        fail $ "ByteArray Ord incorrect"
    unless (arr1 == arr2) $
        fail $ "ByteArray Eq incorrect"
    unless (mappend arr1 arr4 == arr5) $
        fail $ "ByteArray Monoid mappend incorrect"
    unless (mappend arr1 (mappend arr3 arr4) == mappend (mappend arr1 arr3) arr4) $
        fail $ "ByteArray Monoid mappend not associative"
    unless (mconcat [arr1, arr2, arr3, arr4, arr5] == (arr1 <> arr2 <> arr3 <> arr4 <> arr5)) $
        fail $ "ByteArray Monoid mconcat incorrect"
    unless (stimes (3 :: Int) arr4 == (arr4 <> arr4 <> arr4)) $
        fail $ "ByteArray Semigroup stimes incorrect"

instance Arbitrary ByteArray where
  arbitrary = mkByteArray <$> arbitrary

mkByteArray :: [Word8] -> ByteArray
mkByteArray = fromList
