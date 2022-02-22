module Main where


import TUI


import qualified Brick.Main as M


main :: IO ()
main = do
  finalState <- M.defaultMain app initialAppState 
  -- verify _apsInterrupted here to generate or not
  putStrLn $ "final state: " <> show finalState