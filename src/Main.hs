module Main where


import TUI


import qualified Brick.Main as M


main :: IO ()
main = do
  finalState <- M.defaultMain app initialAppState 
  putStrLn $ "final state: " <> show finalState