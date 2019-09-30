module Crossover (
    orderOneCross
) where

import GeneticAlgorithm
import Random

takeChunk :: Int -> Int -> Chromosome -> Chromosome
takeChunk lo hi ch = takeChunkAux lo hi 0 ch where
    takeChunkAux lo hi index [] = []
    takeChunkAux lo hi index (g:ch)
        | index < lo = takeChunkAux lo hi (index+1) ch
        | lo <= index && index <= hi = g:(takeChunkAux lo hi (index+1) ch)
        | otherwise = []

orderOneCrossOP :: Int -> Int -> Chromosome -> Chromosome -> Chromosome
orderOneCrossOP lo hi c1 c2 = insertIntoAux lo hi 0 chu fil where
    chu = takeChunk lo hi c1
    fil = filter (not . (\x -> elem x chu)) c2
    insertIntoAux lo hi childIndex chunk [] = chunk
    insertIntoAux lo hi childIndex [] filtered = filtered
    insertIntoAux lo hi childIndex (ck:chunk) (fi:filtered)
        | lo <= childIndex && childIndex <= hi = ck:(insertIntoAux lo hi (childIndex+1) chunk (fi:filtered))
        | otherwise = fi:(insertIntoAux lo hi (childIndex+1) (ck:chunk) filtered)

orderOneCross :: CrossoverFuntion
orderOneCross seed chromSize c1 c2 = [offspring1, offspring2] where
    (lo, hi) = randIntInterval seed 0 (chromSize-1)
    offspring1 = orderOneCrossOP lo hi c1 c2
    offspring2 = orderOneCrossOP lo hi c2 c1