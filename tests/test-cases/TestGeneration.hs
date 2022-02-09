module TestGeneration where

import Data.Either (isLeft)
import Model (Circuit)
import Generation (generateCircuit)
import System.Random (StdGen, mkStdGen)
import Test.Hspec (Spec, describe, it, pending, shouldBe)
import Test.Hspec.QuickCheck (prop)

import qualified Data.Set as Set

generationTests :: Spec
generationTests = do
  describe "Circuit generation." $ do
    prop "Check it fails if there is no exercises." $ do
      \n -> isLeft (generateCircuit gen Set.empty n circuit) `shouldBe` True
    
    it "Check it fails if there is not enough exercises." $ do
      pending
    
    describe "Generating X rounds on the Y steps circuit." $ do
      it "X steps Y rounds = X*Y total exercises." $ do
        pending
      
      it "X steps Y rounds = X exercises x Y rounds" $ do
        pending

circuit :: Circuit
circuit = undefined

gen :: StdGen
gen = mkStdGen 1