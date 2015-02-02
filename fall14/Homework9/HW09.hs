module HW09 where

import Test.HUnit
import Test.QuickCheck
import Ring
import Data.Functor
import System.Random

instance Arbitrary Mod5 where
    arbitrary = do
        n <- arbitrary
        return $ mkMod n

fromTuple :: (Integer, Integer, Integer, Integer) -> Mat2x2
fromTuple (a, b, c, d) = MkMat a b c d

toTuple :: Mat2x2 -> (Integer, Integer, Integer, Integer)
toTuple (MkMat a b c d) = (a, b, c, d)

instance Arbitrary Mat2x2 where
    arbitrary = fromTuple <$> arbitrary
    shrink mat = map fromTuple $ shrink (toTuple mat)

prop_1 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_1 x y z = (x `add` y) `add` z == x `add` (y `add` z)

prop_2 :: (Ring a, Eq a) => a -> Bool
prop_2 x = (x `add` addId) == x && (addId `add` x) == x

prop_3 :: (Ring a, Eq a) => a -> Bool
prop_3 x = (x `add` (addInv x)) == addId && ((addInv x) `add` x) == addId

prop_4 :: (Ring a, Eq a) => a -> a -> Bool
prop_4 x y = (x `add` y) == (y `add` x)

prop_5 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_5 x y z = (x `mul` y) `mul` z == x `mul` (y `mul` z)

prop_6 :: (Ring a, Eq a) => a -> Bool
prop_6 x = (x `mul` mulId) == x && (mulId `mul` x) == x

prop_7 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_7 x y z = x `mul` (y `add` z) == (x `mul` y) `add` (x `mul` z)

prop_8 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_8 x y z = (y `add` z) `mul` x == (y `mul` x) `add` (z `mul` x)

prop_ring :: (Ring a, Eq a) => a -> a -> a -> Property
prop_ring x y z = conjoin [prop_1 x y z,
                           prop_2 x,
                           prop_3 x,
                           prop_4 x y,
                           prop_5 x y z,
                           prop_6 x,
                           prop_7 x y z,
                           prop_8 x y z]

data BST a = Leaf | Node (BST a) a (BST a)
  deriving Show

genBST :: (Arbitrary a, Random a) => a -> a -> Gen (BST a)
genBST lowerBound upperBound = do
    makeLeaf <- arbitrary
    if makeLeaf
        then return Leaf
        else do
            x <- choose (lowerBound, upperBound)
            leftTree <- genBST lowerBound x
            rightTree <- genBST x upperBound
            return $ Node leftTree x rightTree

parserTests :: Test
parserTests = TestList [parseAll "3" ~?= Just (3 :: Integer),
                        parseAll "3" ~?= Just (MkMod 3),
                        parseAll "5" ~?= Just (MkMod 0),
                        parseAll "[[1,2][3,4]]" ~?= Just (MkMat 1 2 3 4),
                        parseAll "True" ~?= Just True,
                        parseAll "False" ~?= Just False]
