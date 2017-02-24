module DrawShapes where

import Shapes

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.Maybe
import Debug.Trace


--fi :: Int -> Float
fi = fromIntegral

drawRect (R (P (x1, y1)) (P (x2, y2))) =
  let x1' = fi x1
      x2' = fi x2
      y1' = fi y1
      y2' = fi y2
   in fill $ rectangle (V2 x1' y1') (x2' - x1') (y2' - y1')

drawLine (R (P (x1, y1)) (P (x2, y2))) =
  let x1' = fi x1
      x2' = fi x2
      y1' = fi y1
      y2' = fi y2
   in stroke 5 JoinRound (CapRound, CapRound) $ line (V2 x1' y1') (V2 x2' y2')

drawShape (Shape r) =
  let color = PixelRGBA8 0x00 0xBF 0xFF 200
   in withTexture (uniformTexture color) $ drawRect r

drawShape (Obstacle r) =
  let color = PixelRGBA8 0xDC 0xDC 0xDC 200
   in withTexture (uniformTexture color) $ drawRect r

drawShape (Via (P (x, y))) =
  let color = PixelRGBA8 0 0 0xff 255
   in withTexture (uniformTexture color) $
     fill $ circle (V2 (fi x) (fi y)) 5

drawShape (AddedVia (P (x, y))) =
  let color = PixelRGBA8 0x0 0xff 0 255
   in withTexture (uniformTexture color) $
     fill $ circle (V2 (fi x) (fi y)) 5

drawShape (Hline r) =
  let color = PixelRGBA8 0xff 0x00 0x00 200
   in withTexture (uniformTexture color) $ drawLine r

drawShape (Vline r) =
  let color = PixelRGBA8 0xff 0x00 0x00 200
   in withTexture (uniformTexture color) $ drawLine r


--drawShape _ = error "Unimplemented"

drawLayer boundary shapes =
  let R (P (ox, oy)) (P (cx, cy)) = boundary
      white = PixelRGBA8 255 255 255 255
      img = renderDrawing cx cy white $ do
        mapM_ drawShape shapes
   in img

drawProblem :: Problem -> Maybe Solution -> String -> IO ()
drawProblem p ms filename_prefix =
  let mkName n = filename_prefix ++ "_problem_" ++ show n ++ ".png"
      drawL :: (LayerN, [Shape]) -> IO ()
      drawL (LayerN ln, l) = writePng (mkName ln) $ drawLayer (boundary p)  l
      allElements          = pelements p ++ maybe [] (\s -> selements s) ms
      allVias              = pvias p ++ maybe [] (\s -> svias s) ms
      everything           = groupEverything allElements allVias
   in {- traceShow everything $ -} mapM_ drawL everything


