module HW09 where

import Test.QuickCheck
import Ring
import Data.Functor

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

main = do
    putStrLn "prop_1"
    quickCheck $ (prop_1 :: Bool -> Bool -> Bool -> Bool)

    putStrLn "prop_2"
    quickCheck $ (prop_2 :: Bool -> Bool)

    putStrLn "prop_3"
    quickCheck $ (prop_3 :: Bool -> Bool)

    putStrLn "prop_4"
    quickCheck $ (prop_4 :: Bool -> Bool -> Bool)

    putStrLn "prop_5"
    quickCheck $ (prop_5 :: Bool -> Bool -> Bool -> Bool)

    putStrLn "prop_6"
    quickCheck $ (prop_6 :: Bool -> Bool)

    putStrLn "prop_7"
    quickCheck $ (prop_7 :: Bool -> Bool -> Bool -> Bool)

    putStrLn "prop_8"
    quickCheck $ (prop_8 :: Bool -> Bool -> Bool -> Bool)
