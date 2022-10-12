module Board where

import Data.List (transpose)

-------------- Constants
rows :: Int
rows = 6

cols :: Int
cols = 7

win :: Int
win = 4

searchDepth :: Int
searchDepth = 6

-- Types
type Board = [Row]

type Row = [Player]

data Player = O | B | X
  deriving (Ord, Eq, Show)

-- Alias type for clarity
type SearchDepth = Int

-------------- Board transformations
row :: Board -> [Row]
row = id

column :: Board -> [Row]
column = transpose

players :: [Row] -> [[Player]]
players = id

boardToPlayers :: Board -> [[Player]]
boardToPlayers = players . column

diagonals :: [[Player]] -> [[Player]]
-- from data.universe.helpers w slight algorithm improvement
diagonals = drop 1 . go []
  where
    go b es_ =
      [h | h : _ <- b] : case es_ of
        [] -> transpose ts
        e : es -> go (e : ts) es
      where
        ts = [t | _ : t <- b]

--------------  Test Boards
testBoard :: Board
testBoard =
  [ [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, B, X, B, B],
    [B, B, O, O, X, B, B],
    [B, O, O, X, X, X, O]
  ]

testBoard' :: Board
testBoard' =
  [ [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, B, B, O, B],
    [B, B, B, O, X, X, B],
    [B, B, O, O, O, X, X],
    [B, B, O, X, X, O, X]
  ]

-- | Generate blank board for starting point
blank :: Board
blank = replicate rows (replicate cols B)
