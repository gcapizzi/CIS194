{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Buffer
import Data.Monoid

import Sized
import Scrabble
import JoinList

instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ x) = x
    toString xs = init $ unlines $ toList xs

    fromString = fromList . lines

    line = indexJ

    replaceLine 0 line (Single _ _) = scoreLine line
    replaceLine n line buffer@(Append{}) = left +++ scoreLine line +++ right
        where left = takeJ n buffer
              right = dropJ (n + 1) buffer
    replaceLine _ _ buffer = buffer

    numLines Empty = 0
    numLines (Single (_, n) _) = getSize n
    numLines (Append (_, n) _ _) = getSize n

    value Empty = 0
    value (Single (s, _) _) = getScore s
    value (Append (s, _) _ _) = getScore s
