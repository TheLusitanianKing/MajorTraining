module Main where


import TUI.Core
import TUI.AppState

import qualified Brick.Main as M


main :: IO ()
main = do
  finalState <- M.defaultMain app initialAppState
  if _apsInterrupted finalState
    then putStrLn "See you soon!"
    else putStrLn $ "Ready to generate with: " <> show finalState