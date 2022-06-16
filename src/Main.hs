module Main where


import Generation (generateCircuit)
import Model (allExercises)
import Options (Options(..), defaultOptions, getOptions)
import System.Random (initStdGen)
import TUI.Core (app)
import TUI.AppState (initialAppState, validNumberOfSteps, hasBeenInterrupted, getCircuit)

import qualified Brick.Main as M


startApp :: Options -> IO ()
startApp opt = do
  let
    askedNumberOfSteps = _optionInitialNumberOfSteps opt
    initialNumberOfSteps =
      if validNumberOfSteps askedNumberOfSteps
        then askedNumberOfSteps
        else _optionInitialNumberOfSteps defaultOptions
  finalState <- M.defaultMain app $ initialAppState initialNumberOfSteps
  if hasBeenInterrupted finalState
    then putStrLn "See you soon!"
    else do
      gen <- initStdGen
      case generateCircuit gen allExercises (_optionNumberOfRounds opt) (getCircuit finalState) of
        Left errorMessage -> putStrLn errorMessage
        Right generatedCircuit -> print generatedCircuit

main :: IO ()
main = getOptions >>= startApp