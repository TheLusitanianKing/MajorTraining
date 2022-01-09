open Base
open Stdio

type exercise_zone = ZoneBasic | ZoneWall | ZonePullup

type exercise =
  { exercise_name : string
  ; exercise_zone : exercise_zone
  }

let exercises = 
  [ { exercise_name = "Strict Pullups"; exercise_zone = ZonePullup }
  ; { exercise_name = "Chin-ups"; exercise_zone = ZonePullup }
  ; { exercise_name = "Commando Pullups"; exercise_zone = ZonePullup }
  ; { exercise_name = "Towel Pullups"; exercise_zone = ZonePullup }
  ; { exercise_name = "Dips"; exercise_zone = ZonePullup }
  ; { exercise_name = "Burpees Pullups"; exercise_zone = ZonePullup }
  ; { exercise_name = "Toes-to-bar"; exercise_zone = ZonePullup }
  ; { exercise_name = "Full Hanging Leg Wipers"; exercise_zone = ZonePullup }
  ; { exercise_name = "Hanging Knee Wipers"; exercise_zone = ZonePullup }
  ; { exercise_name = "Burpees"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Burpee-Squat-Jump"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Calf Raises"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Climbers"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Froggers"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Supermen"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Hip Raises"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Pikes"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Strict Pushups"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Diamond Pushups"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Handstand Pushups"; exercise_zone = ZoneWall }
  ; { exercise_name = "One-Handed Pushups"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Sphinx Pushups"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Spiderman Pushups"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Jackknives"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Leg Raises"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Leg Wipers"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Situps"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Plank"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Toe-Touch Crunches"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Twists"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Knee Jumps"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Wall Walks"; exercise_zone = ZoneWall }
  ; { exercise_name = "Lunges"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Squats"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Pistols"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Squat Jumps"; exercise_zone = ZoneBasic }
  ; { exercise_name = "Standups"; exercise_zone = ZoneBasic }
  ]