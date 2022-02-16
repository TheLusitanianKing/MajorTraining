{-# LANGUAGE OverloadedStrings #-}

module Model
  ( AppState(..)
  , Equipment(..)
  , Exercise(..)
  , Circuit(..)
  , GeneratedCircuit(..)
  , Step(..)
  , allExercises
  , nbPickedExercises
  )
where


import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set


data Equipment = Wall | PullUpBar | LowPullUpBar
  deriving (Ord, Eq)

data Exercise = Exercise
  { _exerciseName :: Text
  , _exerciseEquipments :: Set Equipment
  } deriving (Ord, Eq)

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

newtype Circuit = Circuit { _circuitSteps :: [Step] }

data GeneratedCircuit = GeneratedCircuit
  { _circuit :: Circuit
  , _rounds :: NonEmpty (NonEmpty Exercise)
  }

newtype AppState = AppState
  { _apsCircuit :: Circuit
  }

-- | Get the total number of exercises from a generated circuit
nbPickedExercises :: GeneratedCircuit -> Int
nbPickedExercises gc = length $ concatMap NE.toList pickedExercises
  where pickedExercises = _rounds gc