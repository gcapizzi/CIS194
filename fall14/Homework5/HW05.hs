module HW05 where

import Ring
import Parser
import Data.List

data Mod5 = Mod5 Integer deriving (Show, Eq)

instance Ring Mod5 where
  addId = Mod5 0
  addInv (Mod5 x) = Mod5 (negate x)
  add (Mod5 x) (Mod5 y) = Mod5 ((x + y) `mod` 5)
  mulId = Mod5 1
  mul (Mod5 x) (Mod5 y) = Mod5 ((x * y) `mod` 5)

instance Parsable Mod5 where
  parse x
    | Just (n, r) <- (parse x) = Just ((Mod5 n), r)
    | otherwise = Nothing

data Mat2x2 = Mat2x2 Integer Integer Integer Integer deriving (Show, Eq)

instance Ring Mat2x2 where
  add (Mat2x2 l11 l12 l21 l22) (Mat2x2 r11 r12 r21 r22) = (Mat2x2 n11 n12 n21 n22)
    where n11 = l11 + r11
          n12 = l12 + r12
          n21 = l21 + r21
          n22 = l22 + r22
  addId = (Mat2x2 0 0 0 0)
  addInv (Mat2x2 a b c d) = (Mat2x2 (negate a) (negate b) (negate c) (negate d))
  mul (Mat2x2 l11 l12 l21 l22) (Mat2x2 r11 r12 r21 r22) = (Mat2x2 n11 n12 n21 n22)
    where n11 = (l11 * r11) + (l12 * r21)
          n12 = (l11 * r12) + (l12 * r22)
          n21 = (l21 * r11) + (l22 * r21)
          n22 = (l21 * r12) + (l22 * r22)
  mulId = (Mat2x2 1 0 0 1)

instance Parsable Mat2x2 where
  parse x = do
    rest <- stripPrefix "[[" x
    (a, rest) <- parse rest
    rest <- stripPrefix "," rest
    (b, rest) <- parse rest
    rest <- stripPrefix "][" rest
    (c, rest) <- parse rest
    rest <- stripPrefix "," rest
    (d, rest) <- parse rest
    rest <- stripPrefix "]]" rest

    return ((Mat2x2 a b c d), rest)

instance Ring Bool where
  add = (/=)
  addId = False
  addInv = id
  mul = (&&)
  mulId = True

instance Parsable Bool where
  parse str
    | Just rest <- stripPrefix "True" str = Just (True, rest)
    | Just rest <- stripPrefix "False" str = Just (False, rest)
    | otherwise = Nothing
