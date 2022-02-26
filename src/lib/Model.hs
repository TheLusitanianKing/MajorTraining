{-# LANGUAGE OverloadedStrings #-}

module Model
  ( Equipment(..)
  , Exercise(..)
  , Circuit(..)
  , GeneratedCircuit(..)
  , Step(..)
  , allEquipments
  , allExercises
  , nbPickedExercises
  )
where


import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as Text


data Equipment = Wall | PullUpBar | LowPullUpBar
  deriving (Bounded, Enum, Eq, Ord)

instance Show Equipment where
  show Wall = "Wall"
  show PullUpBar = "Pull-up Bar"
  show LowPullUpBar = "Low Pull-up Bar"

allEquipments :: [Equipment]
allEquipments = [minBound..maxBound]

data Exercise = Exercise
  { _exerciseName :: Text
  , _exerciseEquipments :: Set Equipment
  } deriving (Eq, Ord, Show)

-- | TODO: all of this could be done automatically from a CSV file or something
--   might be something to change someday
allExercises :: Set Exercise
allExercises = Set.fromList
  [ Exercise { _exerciseName = "Strict Pullups", _exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { _exerciseName = "Chin-ups", _exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { _exerciseName = "Commando Pullups", _exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { _exerciseName = "Towel Pullups", _exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { _exerciseName = "Dips", _exerciseEquipments = Set.fromList [LowPullUpBar] }
  , Exercise { _exerciseName = "Burpees Pullups", _exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { _exerciseName = "Toes-to-bar", _exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { _exerciseName = "Full Hanging Leg Wipers", _exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { _exerciseName = "Hanging Knee Wipers", _exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { _exerciseName = "Burpees", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Burpee-Squat-Jump", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Calf Raises", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Climbers", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Froggers", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Supermen", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Hip Raises", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Pikes", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Strict Pushups", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Diamond Pushups", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Handstand Pushups", _exerciseEquipments = Set.fromList [Wall] }
  , Exercise { _exerciseName = "One-Handed Pushups", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Sphinx Pushups", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Spiderman Pushups", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Jackknives", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Leg Raises", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Leg Wipers", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Situps", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Plank", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Toe-Touch Crunches", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Twists", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Knee Jumps", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Wall Walks", _exerciseEquipments = Set.fromList [Wall] }
  , Exercise { _exerciseName = "Lunges", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Squats", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Pistols", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Squat Jumps", _exerciseEquipments = Set.empty }
  , Exercise { _exerciseName = "Standups", _exerciseEquipments = Set.empty }
  ]

newtype Step = Step { _stepEquipments :: Set Equipment }
  deriving (Eq, Show)

newtype Circuit = Circuit { _circuitSteps :: [Step] }
  deriving (Eq, Show)

data GeneratedCircuit = GeneratedCircuit
  { _circuit :: Circuit
  , _rounds :: NonEmpty (NonEmpty Exercise)
  } deriving (Eq)

instance Show GeneratedCircuit where
  show gc = 
    let
      fill :: Int -> String -> String
      fill x st = replicate x ' ' <> st
      showRounds :: [(Int, [Exercise])] -> String
      showRounds rs =
        unlines
          $ map (\(roundNb, exs) -> showRound roundNb (zip [1..] exs)) rs
      showRound :: Int -> [(Int, Exercise)] -> String
      showRound roundNb exs =
        "Round " <> show roundNb <> ": \n" <> showedExercises
        where
          showedExercises = unlines $ map (fill 2 . uncurry showExercise) exs
      showExercise :: Int -> Exercise -> String
      showExercise nbExercise e =
        "Exercise " <> show nbExercise <> ": " <> Text.unpack (_exerciseName e)
      rounds :: [(Int, [Exercise])]
      rounds = zip [1..] . fmap NE.toList . NE.toList $ _rounds gc
    in showRounds rounds

-- | Get the total number of exercises from a generated circuit
nbPickedExercises :: GeneratedCircuit -> Int
nbPickedExercises gc = length $ concatMap NE.toList pickedExercises
  where pickedExercises = _rounds gc