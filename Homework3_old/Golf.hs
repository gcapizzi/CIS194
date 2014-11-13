module Golf
( skips,
  localMaxima,
  histogram
) where

import Data.List

skips :: [a] -> [[a]]
skips xs = map (flip skip xs) [1..(length xs)]

skip :: Int -> [a] -> [a]
skip d = snd . unzip . filter ((== 0) . (`mod` d) . fst) . zip [1..]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest)
    | y > x && y > z = y:localMaxima (z:rest)
    | otherwise = localMaxima (y:z:rest)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = buildHistogram xs ++ "==========\n0123456789\n"

buildHistogram :: [Integer] -> String
buildHistogram [] = ""
buildHistogram xs = buildHistogram rest ++ starsRow heads ++ "\n"
    where gxs = group $ sort xs
          heads = map head gxs
          rest = concat $ map tail gxs

star :: [Integer] -> Integer -> Char
star xs x
    | x `elem` xs = '*'
    | otherwise = ' '

starsRow xs = map (star xs) [0..9]
