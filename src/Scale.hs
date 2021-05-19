{-# LANGUAGE RecordWildCards #-}
module Scale where

import           Codec.Picture
import qualified Codec.Picture.Types           as M
import           Control.Monad.ST

-- from JuicyPixels-extra, but changed to filter correctly on the edges
scaleBilinear
  :: (Pixel a, Bounded (PixelBaseComponent a), Integral (PixelBaseComponent a))
  => Int               -- ^ Desired width
  -> Int               -- ^ Desired height
  -> Image a           -- ^ Original image
  -> Image a           -- ^ Scaled image
scaleBilinear width height img@Image {..} = runST $ do
  mimg <- M.newMutableImage width height
  let
    sx, sy :: Float
    sx = fromIntegral imageWidth / fromIntegral width
    sy = fromIntegral imageHeight / fromIntegral height
    go x' y'
      | x' >= width = go 0 (y' + 1)
      | y' >= height = M.unsafeFreezeImage mimg
      | otherwise = do
        let
          xf = fromIntegral x' * sx
          yf = fromIntegral y' * sy
          x, y :: Int
          x  = floor xf
          y  = floor yf
          δx = xf - fromIntegral x
          δy = yf - fromIntegral y
          pixelAt' i j =
            pixelAt img (min (pred imageWidth) i) (min (pred imageHeight) j)
        writePixel mimg x' y'
          $      mulp (pixelAt' x y)             ((1 - δx) * (1 - δy))
          `addp` mulp (pixelAt' (x + 1) y)       (δx * (1 - δy))
          `addp` mulp (pixelAt' x (y + 1))       ((1 - δx) * δy)
          `addp` mulp (pixelAt' (x + 1) (y + 1)) (δx * δy)
        go (x' + 1) y'
  go 0 0

{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image Pixel8 -> Image Pixel8 #-}

mulp :: (Pixel a, Integral (PixelBaseComponent a)) => a -> Float -> a
mulp pixel x = colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp #-}

addp
  :: forall a
   . ( Pixel a
     , Bounded (PixelBaseComponent a)
     , Integral (PixelBaseComponent a)
     )
  => a
  -> a
  -> a
addp = mixWith (const f)
 where
  f x y =
    fromIntegral
      $     (maxBound :: PixelBaseComponent a)
      `min` (fromIntegral x + fromIntegral y)
{-# INLINE addp #-}
