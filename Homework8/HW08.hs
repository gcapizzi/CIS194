module HW08 where

import Data.Char
import Data.Maybe
import Data.List
import Data.Monoid
import Text.Read
import Control.Monad
import Control.Monad.Random

digit :: Int -> Maybe Int
digit n
    | n < 10 = Just n
    | otherwise = Nothing

go :: String -> Maybe String
go "" = Just ""
go str = do
    n <- readMaybe (takeWhile isDigit str)
    d <- digit n
    rest <- stripPrefix (replicate d 'a') (dropWhile isDigit str)
    go rest

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go

specialNumbers :: [Int]
specialNumbers = [i | i <- [1..100], i `mod` 5 == 0, i `mod` 7 > 0]

type StdRand = Rand StdGen

type Army = Int
data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army } deriving (Show, Eq)
type DieRoll = Int

dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1,6)

dieRolls :: Int -> StdRand [DieRoll]
dieRolls n = replicateM n dieRoll

instance Monoid ArmyCounts where
    mempty = ArmyCounts { attackers = 0, defenders = 0 }
    mappend x y = ArmyCounts { attackers = attackersSum, defenders = defendersSum }
        where attackersSum = attackers x + attackers y
              defendersSum = defenders x + defenders y

rollResult :: (DieRoll, DieRoll) -> ArmyCounts
rollResult (attackerRoll, defenderRoll)
    | attackerRoll > defenderRoll = ArmyCounts { attackers = 0, defenders = -1 }
    | otherwise = ArmyCounts { attackers = -1, defenders = 0 }

sortDesc = sortBy (flip compare)

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults attackerRolls defenderRolls = mconcat $ map rollResult $ rolls
    where rolls = zip (sortDesc attackerRolls) (sortDesc defenderRolls)

battle :: ArmyCounts -> StdRand ArmyCounts
battle armyCounts = do
    attackerRolls <- dieRolls $ max 3 $ attackers armyCounts - 1
    defenderRolls <- dieRolls $ max 2 $ defenders armyCounts
    return $ mappend armyCounts $ battleResults attackerRolls defenderRolls

invade :: ArmyCounts -> StdRand ArmyCounts
invade armyCounts
    | attackers armyCounts < 2 || defenders armyCounts == 0 = return armyCounts
    | otherwise = battle armyCounts >>= invade

(//) :: Int -> Int -> Double
a // b = fromIntegral a / fromIntegral b

successProb :: ArmyCounts -> StdRand Double
successProb armyCounts = do
    results <- replicateM 1000 $ invade armyCounts
    let wins = length $ filter (\r -> (attackers r > defenders r)) results
    return (wins // 1000)
