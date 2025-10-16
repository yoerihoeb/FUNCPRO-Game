module Main where

import Graphics.Gloss.Interface.IO.Game
import System.Random (newStdGen)

import Controller (step, input)
import Model (initialState)
import View (view)

main :: IO ()
main = do
  gen <- newStdGen
  playIO (InWindow "Shoot'em Up" (1024, 720) (60, 60))
        black
        60
        (initialState gen)
        view
        input
        step
