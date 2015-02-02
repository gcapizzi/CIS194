module HW04 where

import Data.Char
import Data.Maybe
import Data.List

data BST a = Leaf | Node (BST a) a (BST a) deriving (Eq, Show)

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ elem Leaf = Node Leaf elem Leaf
insertBST cmpFn elem (Node left root right) = case (cmpFn elem root) of
    LT -> Node (insertBST cmpFn elem left) root right
    _  -> Node left root (insertBST cmpFn elem right)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

allCaps :: [String] -> Bool
allCaps = all (maybe False isUpper . safeHead)

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile (== ' ') . reverse

firstLetters :: [String] -> [Char]
firstLetters = catMaybes . map safeHead

asList :: [String] -> String
asList xs = "[" ++ concat (intersperse "," xs) ++ "]"
