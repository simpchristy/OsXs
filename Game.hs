module Game where

import Data.Array

data Game = Game { gameState :: State
            , gamePlayer :: Player
            , gameBoard :: Board
            } deriving (Eq, Show)

data State = Running | GameOver (Maybe Player) deriving (Eq, Show)
data Player = PlayerX | PlayerO deriving (Eq, Show)
data Cell = Empty | Full Player deriving (Eq, Show)

type Board = Array (Int,Int) Cell

n :: Int    -- number of cells
n = 9

initialWorld :: Game
initialWorld = Game { gameState = Running 
                    , gamePlayer = PlayerX
                    , gameBoard = initialBoard
                    }

initialBoard :: Board
initialBoard = array boardRange [(,) r Empty | r <- range boardRange]
    where
        boardRange = ((0,0),(2,2))



