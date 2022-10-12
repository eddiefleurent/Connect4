module Game where

import Board
  ( Board,
    Player (..),
    Row,
    boardToPlayers,
    cols,
    column,
    diagonals,
    row,
    win,
  )
import Data.Function (on)
import Data.List (group, maximumBy, transpose)

-------------- Game Logic

-- | Check if board is full
isFull :: Board -> Bool
isFull brd = notElem B $ concat brd

-- | Check the if the player has won
isWin :: Player -> Board -> Bool
isWin p b = not $ null (rs ++ cs ++ ds ++ rds)
  where
    groupP :: [[Player]] -> [[Player]]
    groupP = concatMap group
    filterP :: [[Player]] -> [[Player]]
    filterP xss = [xs | xs <- xss, head xs == p, length xs >= win]
    rs = fp $ row b
    cs = fp $ boardToPlayers b
    ds = fp . diagonals $ boardToPlayers b
    rds = fp . diagonals . reverse $ boardToPlayers b
    fp = filterP . groupP

-- | Change player turn
turn :: Player -> Player
turn X = O
turn O = X
turn _ = error "B player cannot take a turn"

-- | Play a move for each column that is valid
allMoves :: Board -> [Board]
allMoves b
  | isWin p b = []
  | isWin (turn p) b = []
  | isFull b = []
  | otherwise = [b' | i <- [0 .. (cols - 1)], isValidMove i b, let b' = move i b p]
  where
    p :: Player
    p = whosTurn b

-- | Place a stone in the board
move :: Int -> Board -> Player -> Board
move i b p = transpose $ placeRow rTup
  where
    c = column b !! i
    rTup = splitAt i $ column b
    pTup = span (== B) c
    -- \| take a spanned tuple and add the stone based on player
    placeStone :: ([Player], [Player]) -> [Player]
    placeStone (_ : bs, ps) = bs ++ (p : ps)
    -- \| take a split tuple and replace the modified column
    placeRow :: ([Row], [Row]) -> [Row]
    placeRow (bs, _ : ps) = bs ++ (c' : ps)
    c' = placeStone pTup

isValidMove :: Int -> Board -> Bool
isValidMove i b = head r == B
  where
    r = column b !! i

whosTurn :: Board -> Player
whosTurn b =
  if os <= xs -- play function starts with O as 1st player
    then O
    else X
  where
    os = countPlayer O
    xs = countPlayer X
    countPlayer p = length . filter (== p) $ concat b

-- | Calculate number of moves
nMoves :: Board -> Int
nMoves b = length . filter (/= B) $ concat b