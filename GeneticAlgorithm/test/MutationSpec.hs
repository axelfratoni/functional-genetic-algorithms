module MutationSpec (mutationSpec) where

import Test.Hspec
import Test.QuickCheck

import Mutation

mutationSpec :: Spec
mutationSpec = do
    describe "swapMutation" $ do
        it "mutates a chromosome by swapping two genes" $ do
            let chromSize = 1000
            let chrom = [1..chromSize]
            let seed = 432
            let mutatedChrom = swapMutation seed chromSize chrom
            mutatedChrom `shouldMatchList` chrom
            mutatedChrom `shouldNotBe` chrom