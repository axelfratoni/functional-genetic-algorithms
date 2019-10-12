module TravellingSalesman where

import System.Random
import Utils
import Random
import GeneticAlgorithm
import Selection
import Crossover
import Mutation
import Replacement

verifyMat :: [[Int]] -> [[Bool]]
verifyMat mat = [verifyCol i | i<-[0..(rowCount-1)]] where 
    verifyCol col = [mat!!col!!j == mat!!j!!col | j<-[0..(rowCount-1)]]
    rowCount = length mat

isMatWellFormed distanceMat = foldr (&&) True (foldr (\acum l -> (foldr (&&) True l):acum) [] (verifyMat distanceMat))

randomRouteGen :: RandomChromosomeFunction
randomRouteGen chromSize seed = shuffle seed [0..(chromSize-1)]

routeCost :: [[Int]] -> FitnessFunction
routeCost distanceMat [] = 0
routeCost distanceMat (a:[]) = 0
routeCost distanceMat (a:route) = (fromIntegral(getDistance distanceMat a (head route))) + routeCost distanceMat route where
    getDistance mat a b = mat!!a!!b

nextRouteGen :: PopulationSize -> ChromosomeSize -> SelectionSize -> CrossProbability -> FitnessFunction -> NextGenFunction
nextRouteGen cityCount popCount selectionSize crossProb routeCost seed population = nextGeneration 
    cityCount popCount selectionSize crossProb routeCost
    eliteSelector orderOneCross swapMutation simpleReplacemnet
    seed population

routeTermination :: Int -> TerminationFunction
routeTermination maxGenerationNum pop genNum = genNum >= maxGenerationNum