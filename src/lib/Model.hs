{-# LANGUAGE OverloadedStrings #-}

module Model
  ( Equipment(..)
  , Exercise(..)
  , Circuit(..)
  , GeneratedCircuit(..)
  , Step(..)
  , allExercises
  )
where


import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Set as Set


data Equipment = Wall | PullUpBar | LowPullUpBar
  deriving (Ord, Eq)

data Exercise = Exercise
  { exerciseName :: Text
  , exerciseEquipments :: Set Equipment
  } deriving (Ord, Eq)

newtype Step = Step { stepEquipments :: Set Equipment }

newtype Circuit = Circuit { circuitSteps :: [Step] }

data GeneratedCircuit = GeneratedCircuit
  { circuit :: Circuit
  , rounds :: NonEmpty (NonEmpty Exercise)
  }

allExercises :: [Exercise]
allExercises = 
  [ Exercise { exerciseName = "Strict Pullups", exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { exerciseName = "Chin-ups", exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { exerciseName = "Commando Pullups", exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { exerciseName = "Towel Pullups", exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { exerciseName = "Dips", exerciseEquipments = Set.fromList [LowPullUpBar] }
  , Exercise { exerciseName = "Burpees Pullups", exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { exerciseName = "Toes-to-bar", exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { exerciseName = "Full Hanging Leg Wipers", exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { exerciseName = "Hanging Knee Wipers", exerciseEquipments = Set.fromList [PullUpBar] }
  , Exercise { exerciseName = "Burpees", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Burpee-Squat-Jump", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Calf Raises", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Climbers", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Froggers", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Supermen", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Hip Raises", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Pikes", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Strict Pushups", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Diamond Pushups", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Handstand Pushups", exerciseEquipments = Set.fromList [Wall] }
  , Exercise { exerciseName = "One-Handed Pushups", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Sphinx Pushups", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Spiderman Pushups", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Jackknives", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Leg Raises", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Leg Wipers", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Situps", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Plank", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Toe-Touch Crunches", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Twists", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Knee Jumps", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Wall Walks", exerciseEquipments = Set.fromList [Wall] }
  , Exercise { exerciseName = "Lunges", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Squats", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Pistols", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Squat Jumps", exerciseEquipments = Set.empty }
  , Exercise { exerciseName = "Standups", exerciseEquipments = Set.empty }
  ]