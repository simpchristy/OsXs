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
mousePosAsCell (x,y) = ( floor (fromIntegral(n)*(0.5 + x/boardWidth))
                       , floor (fromIntegral(n)*(0.5 + y/boardHeight)) )

mouseClick :: (Float,Float) -> Game -> Game
mouseClick (x,y) game
    |  (x >= fst boardXcoords && x < snd boardXcoords)
    && (y >= fst boardYcoords && y < snd boardYcoords)
        = let cellClick = mousePosAsCell (x,y)
              curPlayer = gamePlayer game
              curBoard = gameBoard game
          in  if (curBoard ! cellClick) == Empty
                then game { gamePlayer = changePlayer curPlayer
                          , gameBoard  = curBoard // [( cellClick, Full curPlayer)] }
                else game
    |  otherwise = game

cells2num :: Cell -> Int
cells2num cell
  | cell == Empty = 0
  | cell == Full PlayerX =  1
  | cell == Full PlayerO = -1

allDirections :: [[(Int,Int)]]
allDirections = concat [hor, ver, dia]
    where hor = [ [(x,y) | x <- [0..n-1]] | y <- [0..n-1] ]
          ver = [ [(x,y) | y <- [0..n-1]] | x <- [0..n-1] ]
          dia = [ [(i,i) | i <- [0..n-1]], [(j,(n-1)-j) | j <- [0..n-1]] ]

checkWinner :: Game -> Game
checkWinner game =
    let board = gameBoard game
        boardP = map (map (cells2num . (!) board)) allDirections
        boardP'= map sum boardP
    in  if   any (\x -> x == 3 || x == -3) boardP'
        then game { gameState = GameOver (Just $ changePlayer $ gamePlayer game) }
        else if   all (\x -> x /= 0) $concat boardP
             then game { gameState = GameOver Nothing }
             else game


eventUpdate :: Event -> Game -> Game
eventUpdate (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Running -> let game' = mouseClick mousePos game
                   in  checkWinner game'
        GameOver _ -> initialWorld
eventUpdate _ game = game




