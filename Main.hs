module Main where

import Game
import Rendering
import Logic
import Graphics.Gloss

window = InWindow "Os & Xs" (640,480) (100,100)
bgColour = makeColor 0 0 0 255
freq = 30


main :: IO ()
main = play window bgColour freq initialWorld world2Picture eventUpdate (const id)
