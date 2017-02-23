module DrawShapes where

import Shapes

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture


fi = fromIntegral

drawRect (R (P (x1, y1)) (P (x2, y2))) =
  let x1' = fi x1
      x2' = fi x2
      y1' = fi y1
      y2' = fi y2
   in fill $ rectangle (V2 x1' y1') (x2' - x1') (y2' - y1')

drawRoutedShape (Shape r) =
  let color = PixelRGBA8 0x00 0xBF 0xFF 200
   in withTexture (uniformTexture color) $ drawRect r


drawVia (Via (P (x, y))) =
  let color = PixelRGBA8 0 0 0xff 255
   in withTexture (uniformTexture color) $
     fill $ circle (V2 (fi x) (fi y)) 5

drawObstacle (Obstacle r) =
  let color = PixelRGBA8 0xDC 0xDC 0xDC 200
   in withTexture (uniformTexture color) $ drawRect r


drawLayer boundary (Layer shapes vias obstacles) =
  let R (P (ox, oy)) (P (cx, cy)) = boundary
      white = PixelRGBA8 255 255 255 255
      img = renderDrawing cx cy white $ do
        mapM_ drawRoutedShape shapes
        mapM_ drawVia vias
        mapM_ drawObstacle obstacles
   in img

drawProblem :: Problem -> String -> IO ()
drawProblem p filename_prefix =
  let mkName n = filename_prefix ++ "_" ++ show n ++ ".png"
      drawL :: (LayerN, Layer) -> IO ()
      drawL (LayerN ln, l) = writePng (mkName ln) $ drawLayer (boundary p)  l
   in mapM_ drawL (splitLayers p)
