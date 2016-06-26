module AParser where

import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
    fmap mapFun = Parser fmap (first mapFun) . runParser

instance Applicative Parser where
  pure x = Parser (\input -> Just(x, input))
  (<*>) (Parser firstParserFun) (Parser secondParserFun) = Parser newParserFun
    where
      newParserFun input = case firstParserFun input of
        Just (f, remainingInput) -> case secondParserFun remainingInput of
          Just (value, restOfInput) -> Just (f value, restOfInput)
          Nothing -> Nothing
        Nothing -> Nothing

abParser = (,) <$> char 'a' <*> char 'b'
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) (Parser p1) (Parser p2) = Parser (\input -> p1 input <|> p2 input)

uppercase = satisfy isUpper
uppercase_ = fmap (const ()) uppercase
posInt_ = fmap (const ()) posInt

intOrUppercase = posInt_ <|> uppercase_
