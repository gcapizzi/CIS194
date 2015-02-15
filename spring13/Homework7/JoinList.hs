module JoinList where

import Sized
import Data.Monoid
import Debug.Trace
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

instance Monoid m => Monoid (JoinList m a) where
    mempty = Empty
    mappend = (+++)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x Empty = x
(+++) Empty y = y
(+++) x y = Append (mappend (tag x) (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single t _) = t
tag (Append t _ _) = t

len :: (Sized b, Monoid b) => JoinList b a -> Int
len Empty = getSize mempty
len (Single s _) = getSize $ size s
len (Append s _ _) = getSize $ size s

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ x) = Just x
indexJ i xs@(Append _ x y)
    | i < len x = indexJ i x
    | i < len xs = indexJ (i - len x) y
indexJ _ _ = Nothing

toList :: JoinList b a -> [a]
toList Empty = []
toList (Single _ x) = [x]
toList (Append _ x y) = toList x ++ toList y

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n xs | n <= 0 = xs
dropJ n (Append s x y)
    | n < len x = dropJ n x +++ y
    | otherwise = dropJ (n - len x) y
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n xs | n <= 0 = Empty
takeJ n (Append s x y)
    | n < len x = takeJ n x
    | otherwise = x +++ takeJ (n - len x) y
takeJ _ xs = xs

scoreLine :: String -> JoinList (Score, Size) String
scoreLine s = Single (scoreString s, Size 1) s

fromList :: [String] -> JoinList (Score, Size) String
fromList [] = Empty
fromList [x] = scoreLine x
fromList xs = fromList left +++ fromList right
    where (left, right) = splitAt (length xs `div` 2) xs
