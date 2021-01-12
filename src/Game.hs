module Game where

import Data.Array

data Game = Game { gameState :: State
            , gamePlayer :: Player
            , gameBoard :: Board
            } deriving (Eq, Show)

data State = Running | GameOver (Maybe Player) deriving (Eq, Show)
data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
type Board = Array (Int,Int) Cell

n :: Int    -- size of grid
n = 3

initialWorld :: Game
initialWorld = Game { gameState = Running 
                    , gamePlayer = PlayerX
                    , gameBoard = initialBoard
                    }

initialBoard :: Board
initialBoard = array boardRange [(,) r Nothing | r <- range boardRange]
    where
        boardRange = ((0,0),(n-1,n-1))



