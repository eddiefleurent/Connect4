module Board where

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

-- >>> blank
-- [[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B],[B,B,B,B,B,B,B]]
blank :: Board
blank = replicate rows (replicate cols B)
