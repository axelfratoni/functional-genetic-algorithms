module Main where

import GeneticAlgorithm
import TravellingSalesman

main :: IO ()
main = travellingSalesmanMain

travellingSalesmanMain :: IO ()
travellingSalesmanMain = if(isMatWellFormed) 
        then do 
            let seed = 432            
            let initialPop = initialPopulation cityCount popCount randomRouteGen seed
            let finalPop = searchGA seed initialPop routeTermination nextRouteGen 0
            let bestRoute = bestChromosome routeCost finalPop
            print "Best route:"
            print bestRoute
            print "Distance:"
            print (routeCost bestRoute)
        else 
            print "Distance matrix is malformed"
