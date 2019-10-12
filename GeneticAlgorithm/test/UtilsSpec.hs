module UtilsSpec (utilsSpec) where

import Test.Hspec
import Test.QuickCheck
import Data.List

import Random
import Utils

utilsSpec :: Spec
utilsSpec = do
    describe "shuffle" $ do
        it "shuffles a list" $ do
            let seed = 432
            let list = [1,2,3,4,5,6,7,8,9]
            let shuffled = Utils.shuffle seed list
            shuffled `shouldMatchList` list
            shuffled `shouldNotBe` list

    describe "takeChunk" $ do
        it "takes a chunk from a chromosome" $ do
            let chrom = [1,2,3,4,5,6,7,8,9]
            let lo = 3
            let hi = 6
            let chunk = takeChunk lo hi chrom
            chunk `shouldBe` [4,5,6,7]
    
    describe "swapAt" $ do
        it "swaps two elements of a chromosome" $ do
            let indexA = 3
            let indexB = 7
            let chrom = [1,2,3,4,5,6,7,8,9]
            let swappedChrom = swapAt indexA indexB chrom
            (chrom!!indexA) `shouldBe` (swappedChrom!!indexB)
            (chrom!!indexB) `shouldBe` (swappedChrom!!indexA)
