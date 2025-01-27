{-# LANGUAGE LambdaCase #-}

module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  , simulate
  ) where

import           Control.Comonad (Comonad (extend, extract))
import           Control.Monad   (liftM2)
import           Data.Grid
import           Data.ListZipper
import           System.Random   (Random (randomR), StdGen, mkStdGen)

-- | Simulation parameters.
data Config = Config
  { probability      :: Double
  , incubationPeriod :: Int
  , illnessDuration  :: Int
  , immunityDuration :: Int
  } deriving Show

-- | State of a cell.
data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

-- | Cell data.
data Cell = Cell
  { cellState :: CellState
  , cellRand  :: StdGen
  }

-- | Simulation grid.
type Comonad19Grid = Grid Cell

-- | By seed generate grid of pseudo random seeds for generators.
--
-- For realization I choose multiplication seed with prime nummbers with overflowing.
genSeedGrid :: Int -> Grid Int
genSeedGrid seed = Grid $ lGenerator (fmap (* 7)) (fmap (* 13)) $ lGenerator (* 19) (* 23) seed

-- | Generate initial state of simulation grid.
initial :: Int -> Comonad19Grid
initial seed = gWrite (Cell (Infected 0) $ mkStdGen seed) $
  Cell Healthy . mkStdGen <$> genSeedGrid seed

-- | Neighbors of cell.
neighbors :: [Grid a -> Grid a]
neighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [gLeft, gRight]
        verticals   = [gUp, gDown]

-- | Check if cell is affected by infection.
isAffected :: Cell -> Bool
isAffected = \case
  Cell (Infected _) _ -> True;
  Cell (Ill _) _ -> True;
  _ -> False

-- | Check if any of neighbors is affected by infection.
affectedNeighbors :: Comonad19Grid -> Bool
affectedNeighbors g = any (isAffected . (\direction -> extract $ direction g)) neighbors

-- | By 'StdGen' and 'Double' probability check happening of event.
tryProbability :: StdGen -> Double -> (Bool, StdGen)
tryProbability gen p = (p' <= p, gen')
  where (p', gen') = randomR (0, 1) gen :: (Double, StdGen)

-- | Rule of infection spread.
rule :: Config -> Comonad19Grid -> Cell
rule config g = case extract g of

  Cell Healthy gen                     ->
    if affectedNeighbors g
    then
      let
        (isInfected, gen') = tryProbability gen (probability config)
      in
        if isInfected
        then Cell (Infected 0) gen'
        else Cell Healthy gen'
    else Cell Healthy gen

  Cell (Infected n) gen
    | n + 1 >= incubationPeriod config -> Cell (Ill 0) gen
  Cell (Infected n) gen                -> Cell (Infected $ n + 1) gen

  Cell (Ill n) gen
    | n + 1 >= illnessDuration config  -> Cell (Immune 0) gen
  Cell (Ill n) gen                     -> Cell (Ill $ n + 1) gen

  Cell (Immune n) gen
    | n + 1 >= immunityDuration config -> Cell Healthy gen
  Cell (Immune n) gen                  -> Cell (Immune $ n + 1) gen

-- | Evolve grid by one step.
evolve :: Config -> Comonad19Grid -> Comonad19Grid
evolve config = extend (rule config)

-- | Creates an infinite list of grids using the given configuration and seed.
-- Each element of this list represents one infection simulation step.
simulate :: Int -> Config -> [Comonad19Grid]
simulate seed config = iterate (evolve config) (initial seed)
