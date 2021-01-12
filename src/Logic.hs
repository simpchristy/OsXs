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
          in  if (curBoard ! cellClick) == Nothing
                then game { gamePlayer = changePlayer curPlayer
                          , gameBoard  = curBoard // [( cellClick, Just curPlayer)] }
                else game
    |  otherwise = game

cells2num :: Cell -> Int
cells2num cell
  | cell == Nothing = 0
  | cell == Just PlayerX =  1
  | cell == Just PlayerO = -1

allDirections :: Board -> [[Int]]
allDirections board = hor ++ ver ++ dia
    where hor = [ [cells2num $ board ! (x,y) | x <- [0..n-1]] | y <- [0..n-1] ]
          ver = [ [cells2num $ board ! (x,y) | y <- [0..n-1]] | x <- [0..n-1] ]
          dia = [ [cells2num $ board ! (i,i) | i <- [0..n-1]]
                , [cells2num $ board ! (j,(n-1)-j) | j <- [0..n-1]] ]

checkWinner :: Game -> Game
checkWinner game =
    let board = gameBoard game
        boardNums =  allDirections board
    in  if   any (\x -> x == 3 || x == -3) $ map sum boardNums
        then game { gameState = GameOver (Just $ changePlayer $ gamePlayer game) }
        else if   all (\x -> x /= 0) $ concat boardNums
             then game { gameState = GameOver Nothing }
             else game


eventUpdate :: Event -> Game -> Game
eventUpdate (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Running -> let game' = mouseClick mousePos game
                   in  checkWinner game'
        GameOver _ -> initialWorld
eventUpdate _ game = game




