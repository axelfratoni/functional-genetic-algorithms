module Main where

import GeneticAlgorithm
import TravellingSalesman

main :: IO ()
main = travellingSalesmanMain

travellingSalesmanMain :: IO ()
travellingSalesmanMain = do
    let distanceMat = [[0, 172, 145, 607, 329, 72, 312, 120], [172, 0, 192, 492, 209, 158, 216, 92], [145, 192, 0, 490, 237, 75, 205, 100], [607, 492, 490, 0, 286, 545, 296, 489], [329, 209, 237, 286, 0, 421, 49, 208], [72, 158, 75, 545, 421, 0, 249, 75], [312, 216, 205, 296, 49, 249, 0, 194], [120, 92, 100, 489, 208, 75, 194, 0]]  
    if(isMatWellFormed distanceMat) then do
        let cityCount = length distanceMat
        let popCount = 100
        let selectionSize = 30
        let crossProb = 0.5
        let maxGenerationNum = 10000
        let seed = 432

        let fitnessFun = routeCost distanceMat
        let nextGenFun = nextRouteGen cityCount popCount selectionSize crossProb fitnessFun
        let terminationFun = routeTermination maxGenerationNum
        let initialPop = initialPopulation cityCount popCount randomRouteGen seed
        let finalPop = searchGA seed initialPop terminationFun nextGenFun 0
        let bestRoute = bestChromosome fitnessFun finalPop

        print "Best route:"
        print bestRoute
        print "Distance:"
        print (fitnessFun bestRoute)

    else 
        print "Distance matrix is malformed"
