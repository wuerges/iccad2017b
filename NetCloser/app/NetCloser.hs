module Main where

import Lib
import System.Environment

import Parser
import Shapes
import DrawShapes
import Graph
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8 as AP

main = do
  [input, output] <- getArgs
  file <- B.readFile input
  case parseOnly parseProblem file of
    Left e -> putStrLn e
    Right p -> Prelude.writeFile output (show (makeSimpleSolution p) ++ "\n")

