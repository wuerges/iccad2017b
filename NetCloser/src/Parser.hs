{-# LANGUAGE OverloadedStrings #-}

module Parser ( parseProblem ) where

import Shapes

import Data.Attoparsec.ByteString.Char8 as AP
import Data.Word
import Data.ByteString
import Data.Map as M


import Debug.Trace

debugParser = do
  r <- takeByteString
  traceShow r $ return ()

isAlphaNum c = isAlpha_ascii c || isDigit c
--isAlphaNum = (||) <$> isAlpha_ascii <*> isDigit


metalLayerN :: Parser LayerN
metalLayerN = do
  char 'M'
  n <- decimal
  ws
  return $ LayerN n

viaLayerN :: Parser LayerN
viaLayerN = do
  char 'V'
  n <- decimal
  ws
  return $ LayerN n


ws :: Parser ()
ws = skipSpace

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
  p2 <- point
  return $ R p1 p2

pintDecl :: ByteString -> Parser Int
pintDecl s = do
  string s
  ws
  char '='
  ws
  d <- decimal
  ws
  return d

pBoundary :: Parser Rect
pBoundary = do
  string "Boundary"
  ws
  char '='
  ws
  r <- rect
  return r

pRoutedShape :: Parser (LayerN, Shape)
pRoutedShape = do
  string "RoutedShape"
  ws
  l <- metalLayerN
  r <- rect
  return (l, Shape r)

pRoutedVia :: Parser (LayerN, Shape)
pRoutedVia = do
  string "RoutedVia"
  ws
  l <- viaLayerN
  p <- point
  return (l, Via p)

pObstacle :: Parser (LayerN, Shape)
pObstacle = do
  string "Obstacle"
  ws
  l <- metalLayerN
  r <- rect
  return (l, Obstacle r)

parseProblem :: Parser Problem
parseProblem = do
  vc <- pintDecl "ViaCost"
  sp <- pintDecl "Spacing"
  b <-  pBoundary
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
    , metalLayers = mls
    , pvias = rvias
    , pelements = shapes ++ obsts }

