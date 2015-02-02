{-# OPTIONS_GHC -fno-warn-missing-methods -XBangPatterns #-}

module HW07 where

import Data.List
import System.Random

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 =  [0,1] ++ zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

instance Show a => Show (Stream a) where
    show xs = show (take 20 $ streamToList xs)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = (Cons (f x) (streamMap f xs))

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed rule seed = (Cons seed (streamFromSeed rule (rule seed)))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = (Cons x (streamInterleave ys xs))

ruler :: Stream Integer
ruler = foldr (streamInterleave) (streamRepeat 123) (map streamRepeat [0..])

randomList :: (Random a, RandomGen g) => g -> [a]
randomList gen = n:randomList newGen
    where (n, newGen) = random gen

randomInts :: Int -> [Int]
randomInts size = take size $ randomList gen
    where gen = mkStdGen 42

minMax :: [Int] -> Maybe (Int, Int) -- 302 MB total memory in use
minMax [] = Nothing
minMax xs = Just (minimum xs, maximum xs)

minMax2 :: [Int] -> Maybe (Int, Int) -- 1 MB total memory in use
minMax2 [] = Nothing
minMax2 (x:xs) = Just $ foldl' (\(!a, !b) n -> (min a n, max b n)) (x, x) xs

data Matrix = Matrix Integer Integer Integer Integer deriving (Eq, Show)

instance Num Matrix where
    (Matrix x11 x12 x21 x22) * (Matrix y11 y12 y21 y22) = Matrix z11 z12 z21 z22
        where z11 = x11 * y11 + x12 * y21
              z12 = x11 * y12 + x12 * y22
              z21 = x21 * y11 + x22 * y21
              z22 = x21 * y12 + x22 * y22
    fromInteger n = Matrix n 0 0 n

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fn
    where (Matrix _ fn _ _) = (Matrix 1 1 1 0) ^ n

main :: IO ()
main = print $ minMax2 $ randomInts 1000000
