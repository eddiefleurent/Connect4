module Main where

import Board (Player (O, X), blank)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import UserInterface (play)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play O blank
