module Generation (generateCircuit) where


import Control.Lens (view)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Model (Circuit(..), Exercise, GeneratedCircuit(..), Step (..), circuitSteps, exerciseEquipments, stepEquipments)
import System.Random (Random(randomR), StdGen)

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set


-- | Generate a circuit as the function clearly says
generateCircuit :: StdGen       -- ^ the random generator
                -> Set Exercise -- ^ the set of exercises to be used in the generated rounds
                -> Int          -- ^ the number of rounds wanted
                -> Circuit      -- ^ the circuit
                -- ^ either fails with an explicit error message or returns the generated circuit
                -> Either String GeneratedCircuit
generateCircuit gen exs nbRounds c
  | null sts = Left "Empty circuit given."
  | otherwise = helper [] [] exs sts gen nbRounds
    where
      sts :: [Step]
      sts = view circuitSteps c
      -- | tail-recursive helper for generating
      helper :: [NonEmpty Exercise] -- ^ the accumulator
             -> [Exercise]          -- ^ the exercises of the current round
             -> Set Exercise        -- ^ the remaining exercises
             -> [Step]              -- ^ the remaining steps
             -> StdGen              -- ^ the current generator
             -> Int                 -- ^ the remaining number of rounds to generate
             -> Either String GeneratedCircuit
      helper acc tmp es steps g n
        | n == 0 && null acc = Left "No rounds generated."
        | n == 0 = Right $ GeneratedCircuit c $ NE.fromList (reverse acc)
        | Set.null es = Left "Not enough exercises to generate the circuit."
        | null steps = helper (NE.fromList (reverse tmp):acc) [] es sts g (n - 1)
        | nbValidExs == 0 = Left "Not enough exercises to generate the circuit."
        | otherwise =
          let
            (i, g') = randomR (0, nbValidExs - 1) g
            pickedExercise = Set.toList validExercises !! i
            remExs = es `Set.difference` Set.fromList [pickedExercise]
          in helper acc (pickedExercise:tmp) remExs (tail steps) g' n
        where
          currentStep = head steps
          validExercises = Set.filter (stepCanHaveThisExercise currentStep) es
          nbValidExs = length validExercises
             

-- | Check if the given step can have the given exercise assigned to it
-- TODO: think about alternatives
-- should it force a step with equipment X to use an exercise with equipment X?
-- or, should it accept whatever exercise that is possible to be done in the step?
-- => should the app always give an exercise using the equipments of the step or not?
stepCanHaveThisExercise :: Step -> Exercise -> Bool
stepCanHaveThisExercise s e = view exerciseEquipments e `Set.isSubsetOf` view stepEquipments s