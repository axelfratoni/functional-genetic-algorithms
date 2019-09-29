module Random (
    Seed,
    randIntList,
    randInt,
    randIntInterval,
    randDoubleList,
    randDouble,
    randDoubleInterval,
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

randIntInterval :: Seed -> Int -> Int -> Int
randIntInterval seed up lo = fst(randomR (lo, up :: Int) (mkStdGen seed))

randDoubleList :: Seed -> [Double]
randDoubleList seed = randoms (mkStdGen seed) :: [Double]

randDouble :: Seed -> Double
randDouble seed = (randDoubleList seed)!!0

randDoubleInterval :: Seed -> Double -> Double -> Double
randDoubleInterval seed up lo = fst(randomR (lo, up :: Double) (mkStdGen seed))

randSeeds :: Seed -> [Seed]
randSeeds seed = randoms (mkStdGen seed) :: [Seed]
