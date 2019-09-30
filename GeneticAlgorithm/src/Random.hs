module Random (
    Seed,
    randIntList,
    randInt,
    randBoundedInt,
    randIntInterval,
    randDoubleList,
    randDouble,
    randBoundedDouble,
    randSeeds
) where

import System.Random
import Data.Time
import Data.Time.Clock.POSIX

type Seed = Int

randIntList :: Seed -> [Int]
randIntList seed = randoms (mkStdGen seed) :: [Int]

randInt :: Seed -> Int
randInt seed = (randIntList seed)!!0

randBoundedInt :: Seed -> Int -> Int -> Int
randBoundedInt seed lo up = fst(randomR (lo, up :: Int) (mkStdGen seed))

randIntInterval :: Seed -> Int -> Int -> (Int, Int)
randIntInterval seed lo up = (left, fst(randomR (left, up :: Int) s2)) where
    (left, s2) = randomR (lo, up :: Int) (mkStdGen seed)

randDoubleList :: Seed -> [Double]
randDoubleList seed = randoms (mkStdGen seed) :: [Double]

randDouble :: Seed -> Double
randDouble seed = (randDoubleList seed)!!0

randBoundedDouble :: Seed -> Double -> Double -> Double
randBoundedDouble seed lo up = fst(randomR (lo, up :: Double) (mkStdGen seed))

randSeeds :: Seed -> [Seed]
randSeeds seed = randoms (mkStdGen seed) :: [Seed]
