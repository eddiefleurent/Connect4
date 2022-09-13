module UserInterface where

import Board (Board, Player (..), cols)

{-
> showBoard testBoard
.......
.......
.......
...XX..
..OOX..
.OOXXXO
-------
0123456
-}

showBoard :: Board -> IO ()
showBoard b =
  putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
  where
    showRow = map showPlayer
    line = replicate cols '-'
    nums = take cols ['0' ..]

testBoard :: Board
testBoard =
  [ [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, X, X, B, B],
    [B, B, O, O, X, B, B],
    [B, O, O, X, X, X, O]
  ]

showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '.'
showPlayer X = 'X'
