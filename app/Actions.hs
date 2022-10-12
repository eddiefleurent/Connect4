module Actions where

import Board (Board, Player (..), cols)
import Data.Char (digitToInt, intToDigit, isDigit)
import Game (isFull, isValidMove, isWin, move, turn)
import Solver (playBot)

-------------- Display

-- | Display Board on screen
showBoard :: Board -> IO ()
showBoard b =
  putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
  where
    showRow = map showPlayer
    line = replicate cols '.'
    nums = take cols ['0' ..]

showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '-'
showPlayer X = 'X'

-------------- User Input

-- | Talk a bit of smack
movePrompt :: Player -> String
movePrompt p = concat ["Show me what you got, player ", show p, "!\nEnter a column number from 0 to ", show (cols - 1), " to drop your stone.\nDon't think too much, you'll lose anyways..."]

validateInt :: String -> Bool
validateInt i
  | length i == 1 && isDigit ih && ih `elem` ['0' .. colsC] = True
  | otherwise = False
  where
    ih = head i -- safe to call head as emptiness is validated
    colsC = intToDigit $ cols - 1

getCol :: Player -> IO Int
getCol p = do
  putStrLn $ movePrompt p
  inp <- getLine
  if validateInt inp
    then pure $ digitToInt $ head inp -- safe head call
    else do
      putStrLn "You expect to beat me, but you can't even follow simple instructions? Try again...\n"
      getCol p

-- | Talk a bit of smack
moveTaunt :: [Char]
moveTaunt = "I looked forward in time to view alternate futures and see all the possible outcomes... I saw 14,000,605 futures, and only one in which you win: The power goes out.\n"

-- | Game loop
play :: Player -> Board -> IO ()
play p b
  | isWin O b = putStrLn "Player O wins!"
  | isWin X b = putStrLn "Player X wins!"
  | isFull b = putStrLn "Somehow you managed to draw..."
  | p == O = do
    -- O is the Bot
    putStrLn moveTaunt
    let b'' = playBot b p
    showBoard b''
    play (turn p) b''
  | otherwise = do
    -- get user input
    i <- getCol p
    if isValidMove i b
      then do
        let b' = move i b p
        showBoard b'
        play (turn p) b'
      else do
        putStrLn "Stop trying to cheat, that is not a valid move. Try again...\n"
        play p b