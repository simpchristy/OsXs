module Logic where

import Game
import Rendering
import Graphics.Gloss.Interface.Pure.Game
import Data.Array

changePlayer :: Player -> Player
changePlayer player = case player of 
    PlayerX -> PlayerO
    PlayerO -> PlayerX

mousePosAsCoord :: (Float,Float) -> (Int,Int)
mousePosAsCoord (x,y) = ( round (fromIntegral(n-1)*x/boardWidth)
                        , round (fromIntegral(n-1)*y/boardHeight) )

mouseClick :: (Float,Float) -> Game -> Game
mouseClick (x,y) game
--    |  (x >= fst boardXcoords && x <= snd boardXcoords)
--    && (y >= fst boardYcoords && y <= snd boardYcoords)
    |  x < 0
        = let cellClick = (0,1) --mousePosAsCoord (x,y)
              curPlayer = gamePlayer game
              curBoard = gameBoard game
          in  game { gamePlayer = changePlayer curPlayer
                   , gameBoard  = curBoard // [( cellClick, Full curPlayer)] }
    |  otherwise = game

eventUpdate :: Event -> Game -> Game
eventUpdate (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Running -> do mouseClick mousePos game
        GameOver (Just player) -> game
        GameOver (Nothing) -> game
eventUpdate _ game = game
