{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid

newtype Score = Score { getScore :: Int } deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score 'a' = 1
score 'b' = 3
score 'c' = 3
score 'd' = 2
score 'e' = 1
score 'f' = 4
score 'g' = 2
score 'h' = 4
score 'i' = 1
score 'j' = 8
score 'k' = 5
score 'l' = 1
score 'm' = 3
score 'n' = 1
score 'o' = 1
score 'p' = 3
score 'q' = 10
score 'r' = 1
score 's' = 1
score 't' = 1
score 'u' = 1
score 'v' = 4
score 'w' = 4
score 'x' = 8
score 'y' = 4
score 'z' = 10
score _ = 0

scoreString :: String -> Score
scoreString = mconcat . map score
