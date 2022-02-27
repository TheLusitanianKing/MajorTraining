module Main where


import Generation (generateCircuit)
import Model (allExercises)
import Options (Options(..), getOptions)
import System.Random (initStdGen)
import TUI.Core (app)
import TUI.AppState (AppState(..), initialAppState)

import qualified Brick.Main as M


startApp :: Options -> IO ()
startApp opt = do
  finalState <- M.defaultMain app (initialAppState $ _optionInitialNumberOfSteps opt)
  if _apsInterrupted finalState
    then putStrLn "See you soon!"
    else do
      gen <- initStdGen
      case generateCircuit gen allExercises (_optionNumberOfRounds opt) (_apsCircuit finalState) of
        Left errorMessage -> putStrLn errorMessage
        Right generatedCircuit -> print generatedCircuit

main :: IO ()
main = getOptions >>= startApp