module Board where

import Data.List (group, transpose)

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

players :: [Row] -> [[Player]]
players = id

boardToPlayers :: Board -> [[Player]]
boardToPlayers = players . column

diagonals :: [[Player]] -> [[Player]]
--from data.universe.helpers
diagonals = drop 1 . go []
  where
    go b es_ =
      [h | h : _ <- b] : case es_ of
        [] -> transpose ts
        e : es -> go (e : ts) es
      where
        ts = [t | _ : t <- b]

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

isWin :: Player -> Board -> Bool
isWin p b = not (null (rs ++ cs ++ ds))
  where
    groupP :: [[Player]] -> [[Player]]
    groupP = group . concat
    filterP :: [[Player]] -> [[Player]]
    filterP xss = [xs | head (head xss) == p, xs <- xss, length xs == win]
    rs = filterP . groupP . players $ row b
    cs = filterP . groupP $ boardToPlayers b
    ds = filterP . groupP . diagonals $ boardToPlayers b