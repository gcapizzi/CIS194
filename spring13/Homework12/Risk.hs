{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (replicateM)
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random            = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle field = do
  let attackerUnits = min (attackers field - 1) 3
  let defenderUnits = min (defenders field) 2
  attackerScores <- sortedDice attackerUnits
  defenderScores <- sortedDice defenderUnits
  let matchups = zipWith compare attackerScores defenderScores
  let attackerWins = count (== GT) matchups
  let defenderWins = count (/= GT) matchups
  let newAttackers = attackers field - defenderWins
  let newDefenders = defenders field - attackerWins
  return Battlefield { attackers = newAttackers, defenders = newDefenders }

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs

sortedDice :: Int -> Rand StdGen [DieValue]
sortedDice n = sortDesc <$> dice n

invade :: Battlefield -> Rand StdGen Battlefield
invade field
  | defenders field == 0 = return field
  | attackers field < 2 = return field
  | otherwise = do
    newField <- battle field
    invade newField

successProb :: Battlefield -> Rand StdGen Double
successProb field = do
  results <- replicateM 1000 (battle field)
  let wins = count attackerWon results
  let loses = count (not . attackerWon) results
  return $ (fromIntegral wins) / (fromIntegral loses)

attackerWon (Battlefield attackers defenders) = attackers > defenders
