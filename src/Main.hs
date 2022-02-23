module Main where


import Generation (generateCircuit)
import Model (allExercises)
import System.Random (initStdGen)
import TUI.Core (app)
import TUI.AppState (AppState(..), initialAppState)

import qualified Brick.Main as M


main :: IO ()
main = do
  finalState <- M.defaultMain app initialAppState
  if _apsInterrupted finalState
    then putStrLn "See you soon!"
    else do
      putStrLn $ "Ready to generate with: " <> show finalState
      gen <- initStdGen
      case generateCircuit gen allExercises 3 (_apsCircuit finalState) of
        Left errorMessage -> putStrLn errorMessage
        Right generatedCircuit -> print generatedCircuit