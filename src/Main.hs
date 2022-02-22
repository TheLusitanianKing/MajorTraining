module Main where


import TUI.Core (app)
import TUI.AppState (AppState(..), initialAppState)

import qualified Brick.Main as M


main :: IO ()
main = do
  finalState <- M.defaultMain app initialAppState
  if _apsInterrupted finalState
    then putStrLn "See you soon!"
    else putStrLn $ "Ready to generate with: " <> show finalState