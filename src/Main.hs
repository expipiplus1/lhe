{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad                  ( guard )
import           Control.Monad.ST               ( runST )
import           Control.Monad.Trans.Maybe
import           Data.Array.Unboxed
import           Data.Bits
import           Data.Foldable
import           Data.STRef
import qualified Data.Vector.Storable          as VS
import           Data.Vector.Unboxed            ( freeze )
import qualified Data.Vector.Unboxed           as VU
import           Data.Vector.Unboxed.Mutable    ( modify
                                                , new
                                                )
import           Data.Word
import           GHC.Conc
import           HopTable
import           Huffman
import           Scale
import           System.Environment
import           System.FilePath

main :: IO ()
main = do
  p <- getArgs >>= \case
    [p] -> pure p
    _   -> fail "Usage: lhe image.png"
  d <- either fail pure =<< readImage p
  let lc :: Image PixelYCbCr8
      lc                         = convertImage . convertRGB8 $ d
      w                          = imageWidth lc
      h                          = imageHeight lc

      -- Split the image into L,Cr,Cb, colors at quarter resolution
      scale                      = scaleBilinear (w `quot` 2) (h `quot` 2)
      upScale                    = scaleBilinear w h
      luma                       = extractComponent PlaneLuma lc
      cb                         = scale $ extractComponent PlaneCb lc
      cr                         = scale $ extractComponent PlaneCr lc

      -- Run LHE on the channels
      (initial, _encodedL, hops) = predict luma
      (_      , encodedCb, _   ) = predict cb
      (_      , encodedCr, _   ) = predict cr
      encodedL                   = decode initial hops

      -- Recombine and save the image
      recombined = encodedL `par` encodedCb `par` encodedCr `par` zipPlanes
        PixelYCbCr8
        encodedL
        (upScale encodedCb)
        (upScale encodedCr)

  let hist = imageHistogram hops
  -- print hist
  -- putStrLn . ppCode . huffmanCode . huffman $ hist
  let code             = huffman hist
      linearHops       = fmap wordToHop . VS.toList . imageData $ hops
      huffmanCoded     = encode code linearHops
      compressedLength = length huffmanCoded
      bpp              = (8 + fi compressedLength) / fi (length linearHops)
  putStrLn . ppCode $ code
  print compressedLength
  print (bpp :: Float)

  writePng (takeBaseName p <> "_recombined.png")
           (convertImage recombined :: Image PixelRGB8)
  writePng (takeBaseName p <> "_encoded_luma.png") encodedL

----------------------------------------------------------------
-- LHE
----------------------------------------------------------------

-- Thoughts:
-- - Better prediction given surrounding pixels
-- - Skew hop selection logarithmically, instead of absolute nearest
-- - Use 9th bit in block ram to indicate if it's a small hop, save a single
--   LUT
-- - No need to store the +1 and -1 hops in the LUT, or the extremal hops when
--   maxHopRange is 1

-- | Takes a monochrome image, returns:
--
-- - The first pixel value
-- - The image as it has been encoded (for debugging)
-- - The hops
predict :: Image Word8 -> (Word8, Image Word8, Image Word8)
predict i = runST $ do
  let w = imageWidth i
      h = imageHeight i

  o        <- newMutableImage w h
  p        <- newMutableImage w h
  aRef     <- newSTRef aInit
  smallHop <- newSTRef False

  writePixel p 0 0 (pixelAt i 0 0)

  -- For every pixel:
  -- - Calculate the average of it's northern and western neighbours
  -- - Find the hop which brings us closest to the target value
  -- - Write that hop value as well as the arrived at value.
  -- - Adjust the `a` parameter
  for_ [0 .. pred h] $ \y -> for_ [(if y == 0 then 1 else 0) .. pred w] $ \x ->
    do
      a  <- runMaybeT $ guard (x /= 0) >> readPixel p (pred x) y
      b  <- runMaybeT $ guard (y /= 0) >> readPixel p x (pred y)
      aX <- readSTRef aRef
      let predicted :: Word8
          predicted       = average a b
          xI              = pixelAt i x y
          (eventual, hop) = bestHop aX predicted xI
      writePixel p x y (fi eventual)
      writePixel o x y (fi $ unHop hop + hopRange)

      wasSmall <- readSTRef smallHop
      let small = isSmall hop
      writeSTRef smallHop small
      if wasSmall && small
        then writeSTRef aRef (max aMin (pred aMin))
        else if not small then writeSTRef aRef aMax else pure ()

  oF <- freezeImage o
  pF <- freezeImage p

  pure (pixelAt i 0 0, pF, oF)

decode :: Word8 -> Image Word8 -> Image Word8
decode initial hops = runST $ do
  let w = imageWidth hops
      h = imageHeight hops

  o <- newMutableImage w h

  writePixel o 0 0 initial

  aRef     <- newSTRef aInit
  smallHop <- newSTRef False

  for_ [0 .. pred h] $ \y -> for_ [(if y == 0 then 1 else 0) .. pred w] $ \x ->
    do
      a  <- runMaybeT $ guard (x /= 0) >> readPixel o (pred x) y
      b  <- runMaybeT $ guard (y /= 0) >> readPixel o x (pred y)
      aX <- readSTRef aRef
      let predicted :: Word8
          predicted = average a b
      let hop    = Hop . subtract hopRange . fi $ pixelAt hops x y
          actual = applyHop aX predicted hop

      writePixel o x y actual

      wasSmall <- readSTRef smallHop
      let small = isSmall hop
      writeSTRef smallHop small
      if wasSmall && small
        then writeSTRef aRef (max aMin (pred aMin))
        else if not small then writeSTRef aRef aMax else pure ()

  freezeImage o

isSmall :: Hop -> Bool
isSmall (Hop i) = i >= (-1) && i <= 1

average :: Maybe Word8 -> Maybe Word8 -> Word8
average lA lB = case (lA, lB) of
  (Nothing, Nothing) -> 0
  (Just a , Nothing) -> a
  (Nothing, Just b ) -> b
  (Just a , Just b ) -> fi ((fi a + fi b :: Word16) `quot` 2)

thresholdHopTable :: ThresholdTable
thresholdHopTable = $$(makeThresholdHopTable hopRange)

-- | returns the hopped-to value and the hop
bestHop :: A -> Word8 -> Word8 -> (Word8, Hop)
bestHop = lookupThresholdHopTable thresholdHopTable

applyHop :: A -> Word8 -> Hop -> Word8
applyHop a xPred hop =
  let t = snd thresholdHopTable
  in
    case hop of
      Hop 0 -> xPred
      Hop i
        | i > 0
        -> fi $ t ! (a, xPred, hop) `shiftR` 8
        | otherwise
        -> let n = fi $ t ! (a, invert xPred, negateHop hop) `shiftR` 8
           in  invert n

----------------------------------------------------------------
-- Huffman
----------------------------------------------------------------

imageHistogram :: Image Word8 -> [(Int, Hop)]
imageHistogram im = runST $ do
  let dat = imageData im
  h <- new (fi hopRange * 2 + 1)
  VS.forM_ dat $ \i -> modify h succ (fi i)
  flip zip (wordToHop <$> [0 ..]) . VU.toList <$> freeze h


----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

wordToHop :: Word8 -> Hop
wordToHop i = Hop (fi i - hopRange)

zipPlanes
  :: (Pixel px, Pixel a, Pixel t1, Pixel t2)
  => (a -> t1 -> t2 -> px)
  -> Image a
  -> Image t1
  -> Image t2
  -> Image px
zipPlanes f a b c = generateImage
  (\x y -> f (pixelAt a x y) (pixelAt b x y) (pixelAt c x y))
  (imageWidth a)
  (imageHeight a)

