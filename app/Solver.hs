module Solver where

import Board (Board, Player (..), blank, cols, rows)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Tree (Tree (Node))
import Game (isValidMove, isWin, move, nMoves, turn, whosTurn)
import Tree (pruneTree)

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