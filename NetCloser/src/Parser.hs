{-# LANGUAGE OverloadedStrings #-}

module Parser ( parseProblem ) where

import Shapes

import Data.Attoparsec.ByteString.Char8 as AP
import Data.Word
import Data.ByteString



identifier :: Parser ByteString
identifier = do
  i <- AP.takeWhile isAlpha_ascii
  ws
  return i

ws :: Parser ()
ws = many' (satisfy isSpace) >> return ()

point :: Parser Point
point = do
  char '('
  ws
  x <- decimal
  ws
  char ','
  ws
  y <- decimal
  ws
  char ')'
  ws
  return $ P (x, y)


rect :: Parser Rect
rect = do
  p1 <- point
  ws
  p2 <- point
  ws
  return $ R p1 p2

pintDecl :: ByteString -> Parser Int
pintDecl s = do
  string s
  ws
  char '='
  ws
  d <- decimal
  endOfLine
  return d

pBoundary :: Parser Rect
pBoundary = do
  string "Boundary"
  ws
  char '='
  ws
  r <- rect
  endOfLine
  return r

pRoutedShape :: Parser Shape
pRoutedShape = do
  string "RoutedShape"
  l <- identifier
  r <- rect
  endOfLine
  return $ Shape (Layer l) r

pRoutedVia :: Parser Via
pRoutedVia = do
  string "RoutedVia"
  ws
  l <- identifier
  p <- point
  endOfLine
  return $ Via (Layer l) p

pObstacle :: Parser Obstacle
pObstacle = do
  string "Obstacle"
  ws
  l <- identifier
  r <- rect
  endOfLine
  return $ Obstacle (Layer l) r



 {-
ViaCost = 20
Spacing = 5
Boundary = (0,0) (1000,1000)
#MetalLayers = 2
#RoutedShapes = 7
#RoutedVias = 1
#Obstacles = 3
RoutedShape M1 (50,100) (250,150)
RoutedShape M1 (600,20) (750,140)
RoutedShape M1 (50,850) (250,900)
RoutedShape M1 (10,800) (500,995)
RoutedShape M2 (75,20) (200,750)
RoutedShape M2 (375,100) (575,600)
RoutedShape M2 (475,20) (670,450)
RoutedVia V1 (175,125)
Obstacle M1 (350,300) (650,750)
Obstacle M1 (50,350) (650,650)
Obstacle M2 (350,700) (950,800)
  -}

parseProblem :: Parser Int --Problem
parseProblem = do
  vc <- pintDecl "ViaCost"
  sp <- pintDecl "Spacing"
  b <-  pBoundary
  return 1
    {-
  mls <- pintDecl "#MetalLayers"
  rss <- pintDecl "#RoutedShapes"
  rvs <- pintDecl "#RoutedVias"
  os <- pintDecl "#Obstacles"
  shapes <- many' pRoutedShape
  rvias <- many' pRoutedVia
  obsts <- many' pObstacle
  endOfInput
  return $ Problem
    { viaCost = vc
    , spacing = sp
    , boundary = b
    , layers = mls
    , routedShapes = shapes
    , routedVias = rvias
    , obstacles = obsts }

     -}


