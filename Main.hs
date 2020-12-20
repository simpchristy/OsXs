module Main where

import Game
import Graphics.Gloss

window = InWindow "Os & Xs" (640,480) (100,100)
bgColour = makeColor 0 0 0 255
freq = 30

--initialWorld = 

main :: IO ()
main = play window bgColour freq initialWorld world2Picture eventUpdate (const gameState)
