module Main where

import Lib
import System.Environment

import Parser

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8 as AP
import DrawShapes

main :: IO ()
main = do
  [input, prefix] <- getArgs
  file <- B.readFile input
  case parseOnly parseProblem file of
    Left e -> putStrLn e
    Right p -> drawProblem p prefix

