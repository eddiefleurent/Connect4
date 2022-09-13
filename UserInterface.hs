module UserInterface where

import Board (Board, Player (..), cols, testBoard)

{-
>>> showBoard testBoard
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

showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '.'
showPlayer X = 'X'
