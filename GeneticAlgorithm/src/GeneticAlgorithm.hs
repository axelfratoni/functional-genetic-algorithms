module GeneticAlgorithm (
    Gene,
    Chromosome,
    Population,
    ChromosomeSize,
    PopulationSize,
    GenerationNumber,
    CrossProbability,
    RandomChromosomeFunction,
    FitnessFunction,
    SelectionFunction,
    CrossoverFuntion,
    MutationFunction,
    ReplacementFunction,
    NextGenFunction,
    TerminationFunction,
    searchGA,
    initialPopulation,
    compareFitness
) where

import Random

type Gene = Int
type Chromosome = [Gene]
type Population = [Chromosome]

type ChromosomeSize = Int
type PopulationSize = Int
type GenerationNumber = Int
type SelectionSize = Int
type CrossProbability = Double

type RandomChromosomeFunction = ChromosomeSize -> Seed -> Chromosome
type FitnessFunction = Chromosome -> Double
type SelectionFunction = SelectionSize -> FitnessFunction -> Population -> Population
type CrossoverFuntion = Seed -> Chromosome -> Chromosome -> [Chromosome]
type MutationFunction = Seed -> Chromosome -> Chromosome
type ReplacementFunction = Population -> Population -> FitnessFunction -> Population
type NextGenFunction = Population -> Population
type TerminationFunction = Population -> GenerationNumber -> Bool

searchGA :: Population -> TerminationFunction -> NextGenFunction -> GenerationNumber -> Population
searchGA population termination nextGen genNum 
    | termination population genNum = population
    | otherwise = searchGA (nextGen population) termination nextGen (genNum+1)

initialPopulation :: ChromosomeSize -> PopulationSize -> RandomChromosomeFunction -> Seed -> Population
initialPopulation chromSize popSize genChrom s = [genChrom chromSize (rs!!i) | i<-[1..popSize]]
    where rs = randSeeds s

crossAll :: CrossoverFuntion -> Seed -> Population -> Population
crossAll cross seed [] = []
crossAll cross seed (c:[]) = []
crossAll cross seed (c:pop) = (cross seed c (head pop)) ++ (crossAll cross seed pop)

mutateAll :: MutationFunction -> Seed -> Population -> Population
mutateAll mutation seed pop = map (mutation seed) pop

nextGeneration :: Seed -> ChromosomeSize -> PopulationSize -> SelectionSize -> CrossProbability -> FitnessFunction -> SelectionFunction -> CrossoverFuntion -> MutationFunction -> ReplacementFunction -> NextGenFunction
nextGeneration seed chromSize popSize k pCross fitFun select cross mutate replace population = replace population (mutateAll (mutate) seed (crossAll (cross) seed (select k fitFun population))) fitFun

compareFitness :: FitnessFunction -> Chromosome -> Chromosome -> Ordering
compareFitness fitFun c1 c2 = compare a b where 
    a = fitFun c1
    b = fitFun c2