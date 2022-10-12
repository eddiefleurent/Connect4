module Main where

import Actions (play)
import Board (Player (O), blank)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play O blank
