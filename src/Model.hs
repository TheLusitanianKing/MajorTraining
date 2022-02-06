{-# LANGUAGE OverloadedStrings #-}

module Model
  ( Equipment(..)
  , Exercise(..)
  , Step(..)
  , Circuit(..)
  , GeneratedCircuit(..)
  , allExercises
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import qualified Data.List.NonEmpty as NE


data Equipment = Wall | PullUpBar | LowPullUpBar

data Exercise = Exercise
  { exerciseName :: Text
  , exerciseEquipments :: [Equipment]
  } 

newtype Step = Step { stepEquipments :: [Equipment] }

newtype Circuit = Circuit { circuitSteps :: [Step] }

data GeneratedCircuit = GeneratedCircuit
  { circuit :: Circuit
  , rounds :: NonEmpty (NonEmpty Exercise)
  }

allExercises :: [Exercise]
allExercises = 
  [ Exercise { exerciseName = "Strict Pullups", exerciseEquipments = [PullUpBar] }
  , Exercise { exerciseName = "Chin-ups", exerciseEquipments = [PullUpBar] }
  , Exercise { exerciseName = "Commando Pullups", exerciseEquipments = [PullUpBar] }
  , Exercise { exerciseName = "Towel Pullups", exerciseEquipments = [PullUpBar] }
  , Exercise { exerciseName = "Dips", exerciseEquipments = [LowPullUpBar] }
  , Exercise { exerciseName = "Burpees Pullups", exerciseEquipments = [PullUpBar] }
  , Exercise { exerciseName = "Toes-to-bar", exerciseEquipments = [PullUpBar] }
  , Exercise { exerciseName = "Full Hanging Leg Wipers", exerciseEquipments = [PullUpBar] }
  , Exercise { exerciseName = "Hanging Knee Wipers", exerciseEquipments = [PullUpBar] }
  , Exercise { exerciseName = "Burpees", exerciseEquipments = [] }
  , Exercise { exerciseName = "Burpee-Squat-Jump", exerciseEquipments = [] }
  , Exercise { exerciseName = "Calf Raises", exerciseEquipments = [] }
  , Exercise { exerciseName = "Climbers", exerciseEquipments = [] }
  , Exercise { exerciseName = "Froggers", exerciseEquipments = [] }
  , Exercise { exerciseName = "Supermen", exerciseEquipments = [] }
  , Exercise { exerciseName = "Hip Raises", exerciseEquipments = [] }
  , Exercise { exerciseName = "Pikes", exerciseEquipments = [] }
  , Exercise { exerciseName = "Strict Pushups", exerciseEquipments = [] }
  , Exercise { exerciseName = "Diamond Pushups", exerciseEquipments = [] }
  , Exercise { exerciseName = "Handstand Pushups", exerciseEquipments = [Wall] }
  , Exercise { exerciseName = "One-Handed Pushups", exerciseEquipments = [] }
  , Exercise { exerciseName = "Sphinx Pushups", exerciseEquipments = [] }
  , Exercise { exerciseName = "Spiderman Pushups", exerciseEquipments = [] }
  , Exercise { exerciseName = "Jackknives", exerciseEquipments = [] }
  , Exercise { exerciseName = "Leg Raises", exerciseEquipments = [] }
  , Exercise { exerciseName = "Leg Wipers", exerciseEquipments = [] }
  , Exercise { exerciseName = "Situps", exerciseEquipments = [] }
  , Exercise { exerciseName = "Plank", exerciseEquipments = [] }
  , Exercise { exerciseName = "Toe-Touch Crunches", exerciseEquipments = [] }
  , Exercise { exerciseName = "Twists", exerciseEquipments = [] }
  , Exercise { exerciseName = "Knee Jumps", exerciseEquipments = [] }
  , Exercise { exerciseName = "Wall Walks", exerciseEquipments = [Wall] }
  , Exercise { exerciseName = "Lunges", exerciseEquipments = [] }
  , Exercise { exerciseName = "Squats", exerciseEquipments = [] }
  , Exercise { exerciseName = "Pistols", exerciseEquipments = [] }
  , Exercise { exerciseName = "Squat Jumps", exerciseEquipments = [] }
  , Exercise { exerciseName = "Standups", exerciseEquipments = [] }
  ]