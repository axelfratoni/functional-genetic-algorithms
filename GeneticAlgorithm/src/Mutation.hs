module Mutation (
    swapMutation
) where

import GeneticAlgorithm
import Random
import Utils

swapMutation :: MutationFunction
swapMutation seed chromSize chrom = swapAt index1 index2 chrom where
    (s1:(s2:rs)) = randSeeds seed
    index1 = randBoundedInt s1 0 (chromSize-1)
    index2 = randBoundedInt s2 0 (chromSize-1)