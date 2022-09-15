module Board where

import Data.List (transpose)

rows :: Int
rows = 6

cols :: Int
cols = 7

win :: Int
win = 4

searchDepth :: Int
--searchDepth = 6
searchDepth = 3

type Board = [Row]

type Row = [Player]

data Player = O | B | X
  deriving (Ord, Eq, Show)

row :: Board -> [Row]
row = id

-- >>> column testBoard
-- [[B,B,B,B,B,B],[B,B,B,B,B,O],[B,B,B,B,O,O],[B,B,B,X,O,X],[B,B,B,X,X,X],[B,B,B,B,B,X],[B,B,B,B,B,O]]
-- >>> span (== B) (column testBoard !! 3)
-- ([B,B,B],[X,O,X])

column :: Board -> [Row]
column = transpose

diagonal :: Board -> [Row]
diagonal = undefined

testBoard :: Board
testBoard =
  [ [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, X, X, B, B],
    [B, B, O, O, X, B, B],
    [B, O, O, X, X, X, O]
  ]

-- >>> blank
-- [[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B]]
blank :: Board
blank = replicate rows (replicate cols B)

-- | Check if board is full
-- >>> isFull blank
-- False
-- >>> isFull (replicate 5 (replicate 6 X))
-- True
isFull :: Board -> Bool
isFull brd = notElem B $ concat brd
