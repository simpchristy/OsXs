module Rendering where

import Graphics.Gloss
import Data.Array
import Game


screenHeight = 480 :: Float
screenWidth = 640 :: Float
boardScale = 0.95
boardHeight = boardScale*screenHeight
boardWidth = boardScale*screenWidth
boardXcoords = ( 0.5*(-boardWidth)  , 0.5*(boardWidth)  )
boardYcoords = ( 0.5*(-boardHeight) , 0.5*(boardHeight) )
cellHeight = boardHeight/3
cellWidth = boardWidth/3

xcolour = red
ocolour = blue
gridcolour = greyN 1.0
tiecolour = greyN 0.5

gameOverColour :: Maybe Player -> Color
gameOverColour (Just PlayerX) = xcolour
gameOverColour (Just PlayerO) = ocolour
gameOverColour Nothing  = tiecolour

shape :: Player -> Picture
shape PlayerX = 
    let width = 10
        length = 120
        rec1 = rotate (-45) $ rectangleSolid width length
        rec2 = rotate 90 rec1
    in  pictures [rec1, rec2]
shape PlayerO =
    let thickness = 10
        radius = 50
    in  thickCircle radius thickness

arr2XCoords :: Int -> Float
arr2XCoords cellno
  | cellno == 0 = -cellWidth
  | cellno == 1 = 0
  | cellno == 2 = cellWidth

arr2YCoords :: Int -> Float
arr2YCoords cellno
  | cellno == 0 = -cellHeight
  | cellno == 1 = 0
  | cellno == 2 = cellHeight

boardPlayer :: Board -> Player -> Picture
boardPlayer board player =
    let board' = assocs board
        boardP = filter (\(_,cell) -> cell == (Full player)) board'
    in  pictures
        $ map (\ ((x,y) ,_)
        -> (translate (arr2XCoords x) (arr2YCoords y) 
        $ shape player)) boardP

xPictures :: Board -> Picture
xPictures board = boardPlayer board PlayerX

oPictures :: Board -> Picture
oPictures board = boardPlayer board PlayerO

gridPicture :: Picture
gridPicture = 
    translate (-boardWidth/2) (-boardHeight/2)
    $ pictures
    $ concatMap (\i -> [ line [(i*cellWidth,0.0),(i*cellWidth,boardHeight)]
                       , line [(0.0,i*cellHeight),(boardWidth,i*cellHeight)]
                       ]
                )
      [0..fromIntegral n]

pictureRunning :: Board -> Picture
pictureRunning board = 
    pictures [ color xcolour $ xPictures board
             , color ocolour $ oPictures board
             , color gridcolour gridPicture
             ]

boardPicture :: Board -> Picture
boardPicture board = 
    pictures [ xPictures board
             , oPictures board
             , gridPicture
             ]

pictureGameOver :: Maybe Player -> Board -> Picture
pictureGameOver winner board = color (gameOverColour winner) (boardPicture board)


world2Picture :: Game -> Picture
world2Picture game =
    case gameState game of
        Running -> pictureRunning (gameBoard game)
        GameOver winner -> pictureGameOver winner (gameBoard game)
