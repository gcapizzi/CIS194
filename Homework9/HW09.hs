module HW09 where

import Test.QuickCheck
import Ring

instance Arbitrary Mod5 where
  arbitrary = do
      n <- arbitrary
      return $ mkMod n

instance Arbitrary Mat2x2 where
  arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ MkMat a b c d
