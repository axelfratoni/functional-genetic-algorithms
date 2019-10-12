module SelectionSpec (selectionSpec) where
    
import Test.Hspec
import Test.QuickCheck

import GeneticAlgorithm
import Selection

selectionSpec :: Spec
selectionSpec = do
  describe "eliteSelector" $ do
    it "selects the first k elements of a population sorted by a sum fitness function" $ do
        let k = 3
        let sumFitness chrom = fromIntegral (foldr (+) 0 chrom)
        let pop = [[9],[8],[7],[6],[5],[4],[3],[2],[1]]
        eliteSelector k sumFitness  pop `shouldBe` [[1],[2],[3]]