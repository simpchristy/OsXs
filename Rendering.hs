module Rendering where

import Graphics.Gloss
import Game

data Symbol = X | O

xcolour = red
ocolour = blue
gridcolour = greyN 1.0
tiecolour = greyN 0.5

gameOverColour :: Maybe Player -> Color
gameOverColour (Just PlayerX) = xcolour
gameOverColour (Just PlayerO) = ocolour
gameOverColour Nothing  = tiecolour

shape :: Symbol -> Picture
shape X = 
        let width = 10
            length = 50
            rec1 = rotate (-45) $ rectangleSolid width length
            rec2 = rotate 90 rec1
        in  pictures [rec1, rec2]
shape O =
        let radius = 5
        in  circleSolid radius

xPictures :: Board -> Picture
xPictures board = shape X

oPictures :: Board -> Picture
oPictures board = shape O

gridPicture :: Picture
gridPicture = blank

boardPicture :: Board -> Picture
boardPicture board = pictures [ xPictures board
                        , oPictures board
                        , gridPicture
                        ]

pictureRunning :: Board -> Picture
pictureRunning board = 
    pictures [ color xcolour $ xPictures board
             , color ocolour $ oPictures board
             , gridPicture
             ]

pictureGameOver :: Maybe Player -> Board -> Picture
pictureGameOver winner board = color (gameOverColour winner) (boardPicture board)


world2Picture :: Game -> Picture
world2Picture game =
    case gameState game of
        Running -> pictureRunning (gameBoard game)
        GameOver winner -> pictureGameOver winner (gameBoard game)
