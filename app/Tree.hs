module Tree where

import Board (Board, SearchDepth, searchDepth)
import Data.Tree (Tree (Node))
import Game (allMoves)

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