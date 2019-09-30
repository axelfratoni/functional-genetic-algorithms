module TravellingSalesman where

import System.Random
import Utils
import Random
import GeneticAlgorithm
import Selection
import Crossover
import Mutation

cityCount = 8
distanceMat = [[0, 172, 145, 607, 329, 73, 312, 120], [172, 0, 192, 492, 209, 158, 216, 92], [145, 192, 0, 490, 237, 75, 205, 100], [607, 492, 490, 0, 286, 545, 296, 489], [329, 209, 237, 286, 0, 421, 49, 208], [72, 158, 75, 545, 421, 0, 249, 75], [312, 216, 205, 296, 49, 249, 0, 194], [120, 92, 100, 489, 208, 75, 194, 0]]

getDistance :: Int -> Int -> Int
getDistance a b = distanceMat!!a!!b

verifyMat :: [[Int]] -> [[Bool]]
verifyMat mat = [verifyCol i | i<-[0..(cityCount-1)]] where 
    verifyCol col = [mat!!col!!j == mat!!j!!col | j<-[0..(cityCount-1)]]

verifiedMat = verifyMat distanceMat
isMatWellFormed = foldr (&&) True (foldr (\acum l -> (foldr (&&) True l):acum) [] verifiedMat)

randomRouteGen :: RandomChromosomeFunction
randomRouteGen chromSize seed = shuffle seed [0..(chromSize-1)]

routeCost :: FitnessFunction
routeCost [] = 0
routeCost (a:[]) = 0
routeCost (a:route) = (fromIntegral(getDistance a (head route))) + routeCost route

popCount = 10
seed = 10
initialPop = initialPopulation cityCount popCount randomRouteGen seed

selected = eliteSelector 3 (routeCost) initialPop
crossed = crossAll orderOneCross seed cityCount selected
mutated = mutateAll swapMutation seed cityCount crossed
