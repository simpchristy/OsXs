module Logic where

import Game
import Rendering
import Graphics.Gloss.Interface.Pure.Game
import Data.Array

changePlayer :: Player -> Player
changePlayer player = case player of 
    PlayerX -> PlayerO
    PlayerO -> PlayerX

mousePosAsCell :: (Float,Float) -> (Int,Int)
mousePosAsCell (x,y) = ( 1 + round (fromIntegral(n-1)*x/boardWidth)
                       , 1 + round (fromIntegral(n-1)*y/boardHeight) )

mouseClick :: (Float,Float) -> Game -> Game
mouseClick (x,y) game
    |  (x >= fst boardXcoords && x <= snd boardXcoords)
    && (y >= fst boardYcoords && y <= snd boardYcoords)
        = let cellClick = mousePosAsCell (x,y)
              curPlayer = gamePlayer game
              curBoard = gameBoard game
          in  if (curBoard ! cellClick) == Empty
                then game { gamePlayer = changePlayer curPlayer
                     , gameBoard  = curBoard // [( cellClick, Full curPlayer)] }
                else game
    |  otherwise = game


eventUpdate :: Event -> Game -> Game
eventUpdate (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Running -> mouseClick mousePos game
        GameOver (Just player) -> game
        GameOver (Nothing) -> game
eventUpdate _ game = game
