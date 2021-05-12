{-# LANGUAGE RecordWildCards #-}
module Main where

import           Codec.Picture
import           Codec.Picture.Extra
import           Codec.Picture.Types
import           Control.Monad                  ( guard )
import           Control.Monad.ST               ( runST )
import           Control.Monad.Trans.Maybe
import           Data.Array.Unboxed
import           Data.Bifunctor                 ( first )
import           Data.Foldable
import           Data.Functor
import           Data.Int
import           Data.Ord
import           Data.STRef
import           Data.Word
import           Debug.Trace
import           System.Environment
import           System.FilePath

main :: IO ()
main = do
  [p] <- getArgs
  d   <- either fail pure =<< readImage p
  let
    lc :: Image PixelYCbCr8
    lc                  = convertImage . convertRGB8 $ d

    w                   = imageWidth lc
    h                   = imageHeight lc
    scale               = scaleBilinear (w `quot` 2) (h `quot` 2)
    upScale             = scaleBilinear w h
    luma                = extractComponent PlaneLuma lc
    cb                  = scale $ extractComponent PlaneCb lc
    cr                  = scale $ extractComponent PlaneCr lc

    -- lheImage                      = lhe luma

    (_, predictedL , _) = predict luma
    (_, predictedCb, _) = predict cb
    (_, predictedCr, _) = predict cr
    recombined          = zipPlanes PixelYCbCr8
                                    predictedL
                                    (upScale predictedCb)
                                    (upScale predictedCr)
  -- writePng (takeBaseName p <> "_luma.png") luma
  -- writePng (takeBaseName p <> "_cr.png")   cr
  -- writePng (takeBaseName p <> "_cb.png")   cb
  writePng (takeBaseName p <> "_recombined.png")
           (convertImage recombined :: Image PixelRGB8)
  -- writePng (takeBaseName p <> "_lhe.png") lheImage
  writePng (takeBaseName p <> "_predicted.png") predictedL
  pure ()

----------------------------------------------------------------
-- LHE
----------------------------------------------------------------

hopRange :: Int8
hopRange = 3

aInit :: A
aInit = A 7
aMin = A 4
aMax = A 10

maxHopRange = 0.3 -- How far to to the max or min bound can we reach

newtype A = A { unA :: Int }
  deriving (Eq, Ord, Ix, Enum)


-- Thoughts:
-- - Better prediction given surrounding pixels
-- - Skew hop selection logarithmically, instead of absolute nearest
--
--

-- lhe :: Image Word8 -> Image Word8
-- lhe i = let predicted = predict i in predicted


predict :: Image Word8 -> (Word8, Image Word8, Image Word8)
predict i = runST $ do
  let w = imageWidth i
      h = imageHeight i
  o        <- newMutableImage w h
  aRef     <- newSTRef aInit
  smallHop <- newSTRef False
  p        <- newMutableImage w h
  writePixel p 0 0 (pixelAt i 0 0)
  sequenceA_
    [ do
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
    | y <- [0 .. pred h]
    , x <- [0 .. pred w]
    , x /= 0 || y /= 0
    ]
  oF <- freezeImage o
  pF <- freezeImage p
  pure (pixelAt i 0 0, pF, oF)

data LocalPixels a = LocalPixels
  { lC :: Maybe (LHEPixel a)
  , lB :: Maybe (LHEPixel a)
  , lD :: Maybe (LHEPixel a)
  , lA :: Maybe (LHEPixel a)
  , lX :: a
  }

data LHEPixel a = Raw a | AHop Hop

newtype Hop = Hop {unHop :: Int8}
  deriving (Eq, Ord, Ix, Show)

isSmall :: Hop -> Bool
isSmall (Hop i) = i >= (-1) && i <= 1

average :: Maybe Word8 -> Maybe Word8 -> Word8
average lA lB = case (lA, lB) of
  (Nothing, Nothing) -> 0
  (Just a , Nothing) -> a
  (Nothing, Just b ) -> b
  (Just a , Just b ) -> fi ((fi a + fi b :: Word16) `quot` 2)

-- hopValue :: Word8 -> Float -> A -> Hop -> Int8
-- hopValue predicted kx a =
--   let n :: Float
--       n  = (255 - fi predicted / kx) ** (1 / kx)

--       p :: Float
--       p  = (fi predicted / kx) ** (1 / kx)

--       go = \case
--         Hop 0    -> 0
--         Hop 1    -> unA a
--         Hop (-1) -> negate (unA a)
--         Hop i | i > 1     -> go (Hop (pred i)) * n
--               | otherwise -> go (Hop (succ i)) * p
--   in  go

hopValue :: A -> Word8 -> Hop -> Float
hopValue (A a) predicted =
  let rangeDivisions = pred hopRange -- The top hop reaches all of `range`

      p =
        (maxHopRange * (255 - fi predicted) / fi a) ** (1 / fi rangeDivisions)
      n = (maxHopRange * fi predicted / fi a) ** (1 / fi rangeDivisions)
  in  \case
        Hop 0 -> 0
        Hop i | i > 0     -> fi a * (p ^ pred i)
              | otherwise -> -fi a * (n ^ pred (abs i))

-- Indexed by (a, predicted, hop)
hopTable :: UArray (A, Word8, Hop) Int16
hopTable =
  let low  = (aMin, 0, Hop (-hopRange))
      high = (aMax, 255, Hop hopRange)
  in  listArray (low, high)
                [ round (hopValue a p h) | (a, p, h) <- range (low, high) ]

-- | returns the hopped-to value and the hop
bestHop :: A -> Word8 -> Word8 -> (Word8, Hop)
bestHop a xPred x =
  let hops  = Hop <$> [-hopRange .. hopRange]
      vals  = (+ fi xPred) <$> [ hopTable ! (a, xPred, h) | h <- hops ]
      diffs = abs . (fi x -) <$> vals
  in  snd $ minimumBy
        (comparing fst)
        (zipWith3 (\d h v -> ((d, abs (unHop h)), (fi v, h))) diffs hops vals)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

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

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
