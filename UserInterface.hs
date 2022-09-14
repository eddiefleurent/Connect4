module UserInterface where

import Board (Board, Player (..), cols)
import Data.Char (digitToInt, intToDigit, isDigit)

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

-- >>> turn B
-- B player cannot take a turn
-- >>> turn X
-- O
-- >>> turn O
-- X
turn :: Player -> Player
turn X = O
turn O = X
turn _ = error "B player cannot take a turn"

movePrompt :: Player -> String
movePrompt p = concat ["Show me what you got, player ", show p, "!\n Enter a column number from 0 to ", show cols, "to drop your stone.\nDon't think too much, you'll lose anyways."]

-- >>> validateInt "1"
-- True
-- >>> validateInt "9"
-- False
-- >>> validateInt "111"
-- False
validateInt :: String -> Bool
validateInt i
  | length i == 1 && isDigit ih && ih `elem` ['0' .. colsC] = True
  | otherwise = False
  where
    ih = head i --safe to call head as emptiness is validated
    colsC = intToDigit cols

getCol :: Player -> IO Int
getCol p = do
  putStrLn $ movePrompt p
  inp <- getLine
  if validateInt inp
    then pure $ digitToInt $ head inp -- safe head call
    else do
      putStrLn "You expect to beat me, but you can't even follow simple instructions? Try again..."
      getCol p
