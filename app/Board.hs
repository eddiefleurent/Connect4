module Board where

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Function (on)
import Data.List (group, maximumBy, transpose)
import Data.Tree (Tree (Node))

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
moveTaunt = "I looked forward in time to view alternate futures to see all the possible outcomes of the coming conflict... I saw 14,000,605 futures, and only one in which you win: The power goes out.\n"

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

-------------- Tree

-- | For each board recursively keep building the tree until the board is full or there is a win
tree :: Board -> Tree Board
tree b = Node b [tree b' | b' <- allMoves b]

-- | Prune based on searchDepth parameter
prune :: SearchDepth -> Tree Board -> Tree Board
prune 0 (Node b _) = Node b []
prune sd (Node b tbs) = Node b [prune (sd - 1) t | t <- tbs]

-- | Produce the game to specified depth
pruneTree :: Board -> Tree Board
pruneTree b = prune searchDepth $ tree b

-------------- Play Bot

-- | Play the perfect game.
-- Negamax with no optimizations, such as this implementation, struggles to find winning branches in the early game.
-- Alpha beta pruning, etc would be the natural progression down the optimization path.
-- The perfect game is encoded for the first moves and negamax takes over after.
-- Inspired by the perfect connect 4 player created by Pascal Pons http://blog.gamesolver.org/solving-connect-four/01-introduction/
playBot :: Board -> Player -> Board
playBot b p
  | not $ null winBoard = head winBoard
  | not $ null defenseNeeded = move (head defenseNeeded) b p
  | b == blank || moves == 2 || moves == 4 = move 3 b p
  | moves == 6 || moves == 16 || moves == 18 || moves == 20 = move' 2
  | moves == 8 || moves == 22 = move' 5
  | moves == 10 || moves == 12 || moves == 14 = move' 4
  | otherwise = bestScore b
  where
    moves = nMoves b
    winBoard = checkWinningMove b p
    defenseNeeded = defense b $ turn p
    move' i
      | isValidMove i b && null silverPlatter = b'
      | otherwise = bestScore b
      where
        -- Check if this move will grant opponent a win
        silverPlatter = defense b' p
        b' = move i b p

-- | Check if any moves will win you the game
checkWinningMove :: Board -> Player -> [Board]
checkWinningMove b p = [move i b p | i <- [0 .. (cols - 1)], isValidMove i b, isWin p (move i b p)]

-- | Check if opponent can win with next move
defense :: Board -> Player -> [Int]
defense b p = [i | i <- [0 .. (cols - 1)], isValidMove i b, isWin p (move i b p)]

-------------- Negamax

type Score = Int

maxMoves :: Int
maxMoves = cols * rows

negamax :: Tree Board -> Tree (Board, Score)
negamax (Node b []) -- Label each leaf with winner, or B if not over or draw
  | isWin X b = Node (b, negate (boardScore b)) [] -- O is the AI so the score is calculated as negative, although traditionally this should alternate based on player
  | isWin O b = Node (b, boardScore b) []
  | otherwise = Node (b, 0) []
negamax (Node b tbs) -- Work down the tree. Alternate min max based on whosTurn
  | whosTurn b == O = Node (b, maximum ss) tbps
  | whosTurn b == X = Node (b, minimum ss) tbps
  where
    tbps = map negamax tbs -- recurse down
    ss = [s | Node (_, s) _ <- tbps] -- extract scores from the tree for comparison
negamax (Node [] (_ : _)) = undefined -- please the compiler, execution path wont be reached
negamax (Node (_ : _) (_ : _)) = undefined -- please the compiler, execution path wont be reached

-- | Position’s score
-- We define a score for any non final position reflecting the outcome of the game for the player to play, considering that both players play perfectly and try to win as soon as possible or lose as late as possible. A position has:
--
-- - a positive score if the current player can win. 1 if he wins with his last stone, 2 if he wins with your second last stone and so on…
-- - a null score if the game will end by a draw game
-- - a negative score if the current player lose whatever he plays. -1 if his opponent wins with his last stone, -2 if his opponent wins with his second last stone and so on…
--
-- Positive (winning) score can be computed as 22 minus number of stone played by the winning at the end of the game.
-- Negative (losing) score can be computed as number of stone played by the winner at the end of the game minus 22.
-- You can notice that after playing the best move, the score of your opponent is the opposite of your score.
-- from: http://blog.gamesolver.org/solving-connect-four/02-test-protocol/
bestScore :: Board -> Board
bestScore b = fst best
  where
    prunedTree = pruneTree b
    Node (_, _) tbps = negamax prunedTree
    -- Get the best score from tree with custom compare function since we have a tuple
    best :: (Board, Score)
    best = maximumBy (compare `on` snd) ([(b', s') | Node (b', s') _ <- tbps, s' >= 0])

-- | Helper function to calculate score if win based on number of moves
winScore :: Int -> Score
winScore mc = (maxMoves - mc) `div` 2

-- | Calculate board score if win based on number of moves
boardScore :: Board -> Score
boardScore = winScore . nMoves
