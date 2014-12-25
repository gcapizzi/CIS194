module HW07 where

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
