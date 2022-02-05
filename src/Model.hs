module Model where

data Equipment = Wall | PullUpBar | LowPullUpBar

data Exercise = Exercise
  { exerciseName :: String
  , exerciseEquipments :: [Equipment]
  } 

newtype Step = Step { stepEquipments :: [Equipment] }

newtype Circuit = Circuit { circuitRounds :: [[(Step, Exercise)]] }

allExercises :: [Exercise]
allExercises = 
  [ Exercise { exercise_name = "Strict Pullups", exercise_equipments = [PullUpBar] }
  , Exercise { exercise_name = "Chin-ups", exercise_equipments = [PullUpBar] }
  , Exercise { exercise_name = "Commando Pullups", exercise_equipments = [PullUpBar] }
  , Exercise { exercise_name = "Towel Pullups", exercise_equipments = [PullUpBar] }
  , Exercise { exercise_name = "Dips", exercise_equipments = [LowPullUpBar] }
  , Exercise { exercise_name = "Burpees Pullups", exercise_equipments = [PullUpBar] }
  , Exercise { exercise_name = "Toes-to-bar", exercise_equipments = [PullUpBar] }
  , Exercise { exercise_name = "Full Hanging Leg Wipers", exercise_equipments = [PullUpBar] }
  , Exercise { exercise_name = "Hanging Knee Wipers", exercise_equipments = [PullUpBar] }
  , Exercise { exercise_name = "Burpees", exercise_equipments = [] }
  , Exercise { exercise_name = "Burpee-Squat-Jump", exercise_equipments = [] }
  , Exercise { exercise_name = "Calf Raises", exercise_equipments = [] }
  , Exercise { exercise_name = "Climbers", exercise_equipments = [] }
  , Exercise { exercise_name = "Froggers", exercise_equipments = [] }
  , Exercise { exercise_name = "Supermen", exercise_equipments = [] }
  , Exercise { exercise_name = "Hip Raises", exercise_equipments = [] }
  , Exercise { exercise_name = "Pikes", exercise_equipments = [] }
  , Exercise { exercise_name = "Strict Pushups", exercise_equipments = [] }
  , Exercise { exercise_name = "Diamond Pushups", exercise_equipments = [] }
  , Exercise { exercise_name = "Handstand Pushups", exercise_equipments = [Wall] }
  , Exercise { exercise_name = "One-Handed Pushups", exercise_equipments = [] }
  , Exercise { exercise_name = "Sphinx Pushups", exercise_equipments = [] }
  , Exercise { exercise_name = "Spiderman Pushups", exercise_equipments = [] }
  , Exercise { exercise_name = "Jackknives", exercise_equipments = [] }
  , Exercise { exercise_name = "Leg Raises", exercise_equipments = [] }
  , Exercise { exercise_name = "Leg Wipers", exercise_equipments = [] }
  , Exercise { exercise_name = "Situps", exercise_equipments = [] }
  , Exercise { exercise_name = "Plank", exercise_equipments = [] }
  , Exercise { exercise_name = "Toe-Touch Crunches", exercise_equipments = [] }
  , Exercise { exercise_name = "Twists", exercise_equipments = [] }
  , Exercise { exercise_name = "Knee Jumps", exercise_equipments = [] }
  , Exercise { exercise_name = "Wall Walks", exercise_equipments = [Wall] }
  , Exercise { exercise_name = "Lunges", exercise_equipments = [] }
  , Exercise { exercise_name = "Squats", exercise_equipments = [] }
  , Exercise { exercise_name = "Pistols", exercise_equipments = [] }
  , Exercise { exercise_name = "Squat Jumps", exercise_equipments = [] }
  , Exercise { exercise_name = "Standups", exercise_equipments = [] }
  ]