{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List
import Data.Char

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy "" _ = True
formableBy (c:word) hand
    | c `elem` hand = formableBy word $ delete c hand
    | otherwise   = False

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate ('?':template) hand (w:word) = w `elem` hand && wordFitsTemplate template (delete w hand) word
wordFitsTemplate (t:template) hand (w:word) = t == w && wordFitsTemplate template hand word
wordFitsTemplate "" _ "" = True
wordFitsTemplate _ _ _ = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = filter (wordFitsTemplate template hand) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord word = sum (map scrabbleValue word)

bestWords :: [String] -> [String]
bestWords words = sort $ filter (\w -> scrabbleValueWord w == maxValue) words
    where maxValue = maximum (map scrabbleValueWord words)

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate template word = (wordFactor template) * (sum squareValues)
    where squareFactors = map squareFactor template
          wordValues = map scrabbleValue word
          squareValues = zipWith (*) squareFactors wordValues

wordFactor :: String -> Int
wordFactor = foldl (*) 1 . map digitToInt . filter (`elem` ['2', '3'])

squareFactor :: Char -> Int
squareFactor 'D' = 2
squareFactor 'T' = 3
squareFactor _ = 1