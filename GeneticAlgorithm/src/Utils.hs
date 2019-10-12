module Utils (
    shuffle,
    takeChunk,
    swapAt
) where

import GeneticAlgorithm
import Random
import System.Random
import qualified Data.Map as Map
import Data.Sort

fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
    where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
    toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
    where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)

shuffle :: Seed -> [a] -> [a]
shuffle s l = fst(fisherYates (mkStdGen s) l)

takeChunk :: Int -> Int -> Chromosome -> Chromosome
takeChunk lo hi ch = takeChunkAux lo hi 0 ch where
    takeChunkAux lo hi index [] = []
    takeChunkAux lo hi index (g:ch)
        | index < lo = takeChunkAux lo hi (index+1) ch
        | lo <= index && index <= hi = g:(takeChunkAux lo hi (index+1) ch)
        | otherwise = []

swapAt :: Int -> Int -> Chromosome -> Chromosome
swapAt a b chrom = swapAux a (chrom!!a) b (chrom!!b) 0 chrom where
    swapAux a elemA b elemB index [] = []
    swapAux a elemA b elemB index (ch:chrom)
        | index == a = elemB:(swapAux a elemA b elemB (index+1) chrom)
        | index == b = elemA:(swapAux a elemA b elemB (index+1) chrom)
        | otherwise = ch:(swapAux a elemA b elemB (index+1) chrom)