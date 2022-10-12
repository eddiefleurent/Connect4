module Main where

import Board (Player (O, X), blank, play, testBoard, testBoard')
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play O blank
