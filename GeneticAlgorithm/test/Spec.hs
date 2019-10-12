import Test.Hspec
import Test.QuickCheck

import SelectionSpec
import CrossoverSpec
import MutationSpec
import ReplacementSpec
import UtilsSpec

main :: IO ()
main = do
    hspec selectionSpec
    hspec crossoverSpec
    hspec mutationSpec
    hspec replacementSpec
    hspec utilsSpec