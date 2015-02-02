{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List

data Market = Market { marketname :: T.Text
                     , state :: T.Text
                     , x :: Double
                     , y :: Double }
    deriving (Show, Eq, Generic)
instance FromJSON Market

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object value) = Object (fmap ynToBool value)
ynToBool (Array value) = Array (fmap ynToBool value)
ynToBool aValue = aValue

parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode

eitherFromJSON :: FromJSON a => Value -> Either String a
eitherFromJSON value = case (fromJSON value) of
    Error msg -> Left msg
    Success res -> Right res

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets str = parseData str >>= eitherFromJSON

loadData :: IO [Market]
loadData = do
    fileData <- B.readFile "markets.json"
    case (parseMarkets fileData) of
        Left msg -> fail msg
        Right markets -> return markets

data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)
instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    mappend (OrdList xs) (OrdList ys) = OrdList (sort $ xs ++ ys)

type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search marketToMonoid query markets = mconcat foundMonoids
    where foundMonoids = map marketToMonoid $ foundMarkets
          foundMarkets = filter (T.isInfixOf query . marketname) markets

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = getFirst `compose2` search (\m -> First (Just m))

lastFound :: Searcher (Maybe Market)
lastFound = getLast `compose2` search (\m -> Last (Just m))

allFound :: Searcher [Market]
allFound = search (\m -> [m])

numberFound :: Searcher Int
numberFound = getSum `compose2` search (\m -> Sum 1)

instance Ord Market where
    compare a b = compare (y a) (y b)

orderedNtoS :: Searcher [Market]
orderedNtoS = getOrdList `compose2` search (\m -> OrdList [m])
