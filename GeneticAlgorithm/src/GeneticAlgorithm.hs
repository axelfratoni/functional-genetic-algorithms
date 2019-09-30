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
    crossAll
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
type ReplacementFunction = FitnessFunction -> Population -> Population -> Population
type NextGenFunction = Population -> Population
type TerminationFunction = Population -> GenerationNumber -> Bool

searchGA :: Population -> TerminationFunction -> NextGenFunction -> GenerationNumber -> Population
searchGA population termination nextGen genNum 
    | termination population genNum = population
    | otherwise = searchGA (nextGen population) termination nextGen (genNum+1)

initialPopulation :: ChromosomeSize -> PopulationSize -> RandomChromosomeFunction -> Seed -> Population
initialPopulation chromSize popSize genChrom s = [genChrom chromSize (rs!!i) | i<-[1..popSize]]
    where rs = randSeeds s

crossAll :: CrossoverFuntion -> Seed -> ChromosomeSize -> Population -> Population
crossAll cross seed chromSize [] = []
crossAll cross seed chromSize (c:[]) = [c]
crossAll cross seed chromSize (c:pop) = (cross s1 chromSize c (head pop)) ++ (crossAll cross s2 chromSize (tail pop))
    where (s1:(s2:rs)) = randSeeds seed

mutateAll :: MutationFunction -> Seed -> ChromosomeSize -> Population -> Population
mutateAll mutation seed chromSize [] = []
mutateAll mutation seed chromSize (chr:pop) = (mutation s1 chromSize chr):(mutateAll mutation s2 chromSize pop)
    where (s1:(s2:rs)) = randSeeds seed

nextGeneration :: Seed -> ChromosomeSize -> PopulationSize -> SelectionSize -> CrossProbability -> FitnessFunction -> SelectionFunction -> CrossoverFuntion -> MutationFunction -> ReplacementFunction -> NextGenFunction
nextGeneration seed chromSize popSize k pCross fitFun select cross mutate replace population = 
    replace fitFun population
    (mutateAll (mutate) seed chromSize 
    (crossAll (cross) seed chromSize 
    (select k fitFun population))) 
    

compareFitness :: FitnessFunction -> Chromosome -> Chromosome -> Ordering
compareFitness fitFun c1 c2 = compare a b where 
    a = fitFun c1
    b = fitFun c2