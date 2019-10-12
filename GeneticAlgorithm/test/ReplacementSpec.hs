module ReplacementSpec (replacementSpec) where
    
import Test.Hspec
import Test.QuickCheck

import GeneticAlgorithm
import Replacement

replacementSpec :: Spec
replacementSpec = do
    describe "simpleReplacemnet" $ do
        it "applies simple replacement to a population" $ do
            let k = 3
            let sumFitness chrom = fromIntegral (foldr (+) 0 chrom)
            let pop = [[9],[8],[7],[6],[5],[4],[3],[2],[1]]
            let offsprings = [[1,2],[2,3],[3,4]]
            let seed = 432
            let replacedPop = simpleReplacemnet seed sumFitness k pop offsprings
            replacedPop `shouldContain` offsprings