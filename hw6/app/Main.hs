{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Data.Grid
import           Data.ListZipper
import           HW6.T3
import           Options.Applicative
import           System.Random

-- | Converts 'CellState' to 'Char' for displaying of simulation.
viewCellState :: CellState -> Char
viewCellState = \case
  Healthy      -> '_'
  Infected _   -> 'i'
  Ill _        -> '#'
  Immune _     -> '@'

-- | Frame 'ListZipper' to with given width.
frameLZ :: Int -> ListZipper a -> [a]
frameLZ n (LZ ls x rs) = reverse (take lTake ls) ++ [x] ++ take (n - 1 - lTake) rs
  where lTake = n `div` 2

-- | Frame of 'Comonad19Grid'.
type Comonad19Frame = [[Cell]]

-- | Converts 'Comonad19Grid' to 'Comonad19Frame' with given width.
frame :: Int -> Comonad19Grid -> Comonad19Frame
frame n = map (frameLZ n) . frameLZ n . unGrid

-- | Converts 'Comonad19Frame' to 'String' for displaying.
frameToString :: Comonad19Frame -> String
frameToString = unlines . map (map (viewCellState . cellState))

-- | Simulation options, reading from command line.
data SimulationOptions = SimulationOptions
  { config     :: Config
  , gridSize   :: Int
  , iterations :: Int
  } deriving (Show)

-- | Parser of 'Config' fields from command line.
configParser :: Parser Config
configParser = Config
  <$> option auto
      ( long "prob"
     <> metavar "INFECTION_PROBABILITY"
     <> help "Probability of infection" )
  <*> option auto
      ( long "incub"
     <> metavar "INCUBATION_DURATION"
     <> help "Incubation duration" )
  <*> option auto
      ( long "ill"
     <> metavar "ILLNESS_DURATION"
     <> help "Illness duration" )
  <*> option auto
      ( long "immun"
     <> metavar "IMMUNITY_DURATION"
     <> help "Immunity duration" )

-- | Parser of 'SimulationOptions' from command line.
simulationOptionsParser :: Parser SimulationOptions
simulationOptionsParser = SimulationOptions
  <$> configParser
  <*> option auto
      ( long "grid-size"
     <> metavar "GRID_SIZE"
     <> help "Grid size" )
  <*> option auto
      ( long "iterations"
     <> metavar "ITERATIONS"
     <> help "Iterations number of simulation" )

-- | Command line options parser.
opts :: ParserInfo SimulationOptions
opts = info (simulationOptionsParser <**> helper)
  ( fullDesc
  <> progDesc "Run a simulation with the given parameters"
  <> header "comonad19 - An infection simulation program" )

-- | Runs simulation with given configuration and seed.
runSimulation :: Int -> SimulationOptions -> [Comonad19Frame]
runSimulation seed (SimulationOptions conf size iters) =
  map (frame size) (take iters $ simulate seed conf)

-- | Runs simulation with random seed from system and configuration from command line.
main :: IO ()
main = do
  options <- execParser opts
  seed    <- randomIO :: IO Int
  mapM_ (putStrLn . frameToString) (runSimulation seed options)
