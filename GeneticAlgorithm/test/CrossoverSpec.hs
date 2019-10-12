module CrossoverSpec (crossoverSpec) where

import Test.Hspec
import Test.QuickCheck

import GeneticAlgorithm
import Crossover

crossoverSpec :: Spec
crossoverSpec = do
    describe "orderOneCross" $ do
        it "crosses two cromosomes with order one method" $ do
            let chrom1 = [1,2,3,4,5,6,7,8,9]
            let chrom2 = [9,8,7,6,4,5,3,2,1]
            let chromSize = 9
            let seed = 432
            let (offSpring1:(offSpring2:xs)) = orderOneCross seed chromSize chrom1 chrom2
            offSpring1 `shouldNotBe` chrom1
            offSpring1 `shouldNotBe` chrom2
            offSpring2 `shouldNotBe` chrom1
            offSpring2 `shouldNotBe` chrom2
            offSpring1 `shouldMatchList` chrom1
            offSpring1 `shouldMatchList` chrom2
            offSpring2 `shouldMatchList` chrom1
            offSpring2 `shouldMatchList` chrom2