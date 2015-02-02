module Wholemeal
( fun1,
  fun2,
  xor,
  map',
  sieveSundaram,
) where

import Data.List

fun1 :: [Integer] -> Integer
fun1 = foldl (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

xor :: [Bool] -> Bool
xor = foldl1 xor'

xor' True = not
xor' False = id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map doubleAndInc $ withoutComposites n
    where doubleAndInc = (+1) . (*2)

composites n = [composite i j | i <- [1..n], j <- [1..i], composite i j <= n]
    where composite i j = i + j + 2 * i * j
withoutComposites n = [1..n] \\ composites n
