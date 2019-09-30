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
    compareFitness,
    nextGeneration
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
type CrossoverFuntion = Seed -> ChromosomeSize -> Chromosome -> Chromosome -> [Chromosome]
type MutationFunction = Seed -> ChromosomeSize -> Chromosome -> Chromosome
type ReplacementFunction = Seed -> FitnessFunction -> SelectionSize -> Population -> Population -> Population
type NextGenFunction = Seed -> Population -> Population
type TerminationFunction = Population -> GenerationNumber -> Bool

searchGA :: Seed -> Population -> TerminationFunction -> NextGenFunction -> GenerationNumber -> Population
searchGA seed population termination nextGen genNum 
    | termination population genNum = population
    | otherwise = searchGA s1 (nextGen s2 population) termination nextGen (genNum+1)
    where (s1:(s2:rs)) = randSeeds seed

initialPopulation :: ChromosomeSize -> PopulationSize -> RandomChromosomeFunction -> Seed -> Population
initialPopulation chromSize popSize genChrom s = [genChrom chromSize (rs!!i) | i<-[1..popSize]]
    where rs = randSeeds s

crossAll :: CrossoverFuntion -> Seed -> CrossProbability -> ChromosomeSize -> Population -> Population
crossAll cross seed pCross chromSize [] = []
crossAll cross seed pCross chromSize (c:[]) = [c]
crossAll cross seed pCross chromSize (c:pop) 
    | pCross > (randBoundedDouble s3 0 1) = (cross s1 chromSize c (head pop)) ++ (crossAll cross s2 pCross chromSize (tail pop))
    | otherwise = (c:pop) ++ (crossAll cross s2 pCross chromSize (tail pop))
    where (s1:(s2:(s3:rs))) = randSeeds seed

mutateAll :: MutationFunction -> Seed -> ChromosomeSize -> Population -> Population
mutateAll mutation seed chromSize [] = []
mutateAll mutation seed chromSize (chr:pop) = (mutation s1 chromSize chr):(mutateAll mutation s2 chromSize pop)
    where (s1:(s2:rs)) = randSeeds seed

nextGeneration :: ChromosomeSize -> PopulationSize -> SelectionSize -> CrossProbability -> FitnessFunction -> SelectionFunction -> CrossoverFuntion -> MutationFunction -> ReplacementFunction -> NextGenFunction
nextGeneration chromSize popSize selecSize pCross fitFun select cross mutate replace seed population = 
    replace s3 fitFun selecSize population
    (mutateAll mutate s2 chromSize 
    (crossAll cross s1 pCross chromSize 
    (select selecSize fitFun population))) 
    where (s1:(s2:(s3:rs))) = randSeeds seed
    
compareFitness :: FitnessFunction -> Chromosome -> Chromosome -> Ordering
compareFitness fitFun c1 c2 = compare a b where 
    a = fitFun c1
    b = fitFun c2