module CreditCard
( toDigits,
  toDigitsRev,
  doubleEveryOther,
  sumDigits,
  validate
) where

import Data.Char

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = map (toInteger . digitToInt) $ show x

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft xs = zipWith (*) xs (cycle [1, 2])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherFromLeft (reverse xs)

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 $ concatMap toDigits xs

checksum :: Integer -> Integer
checksum = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate x = checksum x `rem` 10 == 0
