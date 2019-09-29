module Selection (
    eliteSelector
) where

import GeneticAlgorithm
import Data.Sort

eliteSelector :: SelectionFunction
eliteSelector k fitFun pop = take k (sortBy (compareFitness fitFun) pop)

