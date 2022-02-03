open Base

type equipment = Wall | PullUpBar | LowPullUpBar

type exercise =
  { exercise_name : string
  ; exercise_equipments : equipment list
  }

type step = { step_equipments : equipment list }

type circuit = { circuit_rounds : (step * exercise) list list }

let all_exercises = 
  [ { exercise_name = "Strict Pullups"; exercise_equipments = [PullUpBar] }
  ; { exercise_name = "Chin-ups"; exercise_equipments = [PullUpBar] }
  ; { exercise_name = "Commando Pullups"; exercise_equipments = [PullUpBar] }
  ; { exercise_name = "Towel Pullups"; exercise_equipments = [PullUpBar] }
  ; { exercise_name = "Dips"; exercise_equipments = [LowPullUpBar] }
  ; { exercise_name = "Burpees Pullups"; exercise_equipments = [PullUpBar] }
  ; { exercise_name = "Toes-to-bar"; exercise_equipments = [PullUpBar] }
  ; { exercise_name = "Full Hanging Leg Wipers"; exercise_equipments = [PullUpBar] }
  ; { exercise_name = "Hanging Knee Wipers"; exercise_equipments = [PullUpBar] }
  ; { exercise_name = "Burpees"; exercise_equipments = [] }
  ; { exercise_name = "Burpee-Squat-Jump"; exercise_equipments = [] }
  ; { exercise_name = "Calf Raises"; exercise_equipments = [] }
  ; { exercise_name = "Climbers"; exercise_equipments = [] }
  ; { exercise_name = "Froggers"; exercise_equipments = [] }
  ; { exercise_name = "Supermen"; exercise_equipments = [] }
  ; { exercise_name = "Hip Raises"; exercise_equipments = [] }
  ; { exercise_name = "Pikes"; exercise_equipments = [] }
  ; { exercise_name = "Strict Pushups"; exercise_equipments = [] }
  ; { exercise_name = "Diamond Pushups"; exercise_equipments = [] }
  ; { exercise_name = "Handstand Pushups"; exercise_equipments = [Wall] }
  ; { exercise_name = "One-Handed Pushups"; exercise_equipments = [] }
  ; { exercise_name = "Sphinx Pushups"; exercise_equipments = [] }
  ; { exercise_name = "Spiderman Pushups"; exercise_equipments = [] }
  ; { exercise_name = "Jackknives"; exercise_equipments = [] }
  ; { exercise_name = "Leg Raises"; exercise_equipments = [] }
  ; { exercise_name = "Leg Wipers"; exercise_equipments = [] }
  ; { exercise_name = "Situps"; exercise_equipments = [] }
  ; { exercise_name = "Plank"; exercise_equipments = [] }
  ; { exercise_name = "Toe-Touch Crunches"; exercise_equipments = [] }
  ; { exercise_name = "Twists"; exercise_equipments = [] }
  ; { exercise_name = "Knee Jumps"; exercise_equipments = [] }
  ; { exercise_name = "Wall Walks"; exercise_equipments = [Wall] }
  ; { exercise_name = "Lunges"; exercise_equipments = [] }
  ; { exercise_name = "Squats"; exercise_equipments = [] }
  ; { exercise_name = "Pistols"; exercise_equipments = [] }
  ; { exercise_name = "Squat Jumps"; exercise_equipments = [] }
  ; { exercise_name = "Standups"; exercise_equipments = [] }
  ]