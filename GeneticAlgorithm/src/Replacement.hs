module Replacement (
    simpleReplacemnet
) where

import GeneticAlgorithm
import Random
import Utils

simpleReplacemnet :: ReplacementFunction
simpleReplacemnet seed fitFun selecSize population offsprings = 
    offsprings ++ (drop selecSize (shuffle seed population))
