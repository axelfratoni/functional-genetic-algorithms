module Mutation (
    swapMutation
) where

import GeneticAlgorithm
import Random

swapAt :: Int -> Int -> Chromosome -> Chromosome
swapAt a b chrom = swapAux a (chrom!!a) b (chrom!!b) 0 chrom where
    swapAux a elemA b elemB index [] = []
    swapAux a elemA b elemB index (ch:chrom)
        | index == a = elemB:(swapAux a elemA b elemB (index+1) chrom)
        | index == b = elemA:(swapAux a elemA b elemB (index+1) chrom)
        | otherwise = ch:(swapAux a elemA b elemB (index+1) chrom)

swapMutation :: MutationFunction
swapMutation seed chromSize chrom = swapAt index1 index2 chrom where
    (s1:(s2:rs)) = randSeeds seed
    index1 = randBoundedInt s1 0 chromSize
    index2 = randBoundedInt s2 0 chromSize