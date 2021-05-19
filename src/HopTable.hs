{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module HopTable where

import           Data.Array.Unboxed
import           Data.Bits
import           Data.Foldable                  ( asum )
import           Data.Int
import           Data.Maybe
import           Data.Word
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( Lift
                                                , liftTyped
                                                )
import Control.Monad
import Data.Functor

-- The 'a' parameter, the distance of the first hop away from 0
aInit :: A
aInit = A 7
aMin :: A
aMin = A 4
aMax :: A
aMax = A 10

hopRange :: Int8
hopRange = 3

-- The uppermost hop will move this far towards black or white
maxHopRange :: Float
maxHopRange = 0.9 -- How far to to the max or min bound can we reach


newtype A = A { unA :: Int }
  deriving (Eq, Ord, Ix, Enum, Lift, Show)

newtype Hop = Hop {unHop :: Int8}
  deriving (Eq, Ord, Ix, Enum, Show, Lift)

hopValue
  :: Int8
  -- ^ Number of hops each side of 0
  -> A
  -- ^ First hop distance
  -> Word8
  -- ^ Predicted value
  -> Hop
  -- ^ The Hop
  -> Float
  -- ^ The hop distance
hopValue hopRange (A a) predicted =
  let rangeDivisions = pred hopRange -- The top hop reaches all of `hopRange`

      p =
        (maxHopRange * (255 - fi predicted) / fi a) ** (1 / fi rangeDivisions)

      n = (maxHopRange * fi predicted / fi a) ** (1 / fi rangeDivisions)
  in  \case
        Hop 0 -> 0
        Hop i | i > 0     -> fi a * (p ^ pred i)
              | otherwise -> -fi a * (n ^ pred (abs i))

-- Indexed by (a, predicted, hop)
makeHopTable :: Int8 -> Q (TExp (UArray (A, Word8, Hop) Int16))
makeHopTable hopRange =
  let low  = (aMin, 0, Hop (-hopRange))
      high = (aMax, 255, Hop hopRange)
  in  [||listArray (low, high)
        $$(liftTyped [ round (hopValue hopRange a p h) | (a, p, h) <- range (low, high) ])||]

-- | Store the hopped to value in the upper 8 bits and the lower bound in the
-- lower 8 bits
makeThresholdHopTable :: Int8 -> Q (TExp ThresholdTable)
makeThresholdHopTable hopRange =
  let
    low  = (aMin, 0, Hop 1)
    high = (aMax, 255, Hop hopRange)
    dat =
      [ fi v `shiftL` 8 .|. fi t
      | (a, p, h) <- range (low, high)
      , let
        v = floatToWord8 $ fi p + hopValue hopRange a p h
        t = floatToWord8 $ fi p + hopValue (hopRange * 2)
                                           a
                                           p
                                           (Hop $ unHop h * 2 - 1)
      ]
  in
    [||(hopRange, listArray (low, high) $$(liftTyped dat))||]

type ThresholdTable = (Int8, UArray (A, Word8, Hop) Word16)

lookupThresholdHopTable
  :: ThresholdTable -> A -> Word8 -> Word8 -> (Word8, Hop)
lookupThresholdHopTable (hopRange, table) a xPred x = if x >= xPred
  then
    let candidates =
          [ (h, fi (r `shiftR` 8), fi r)
          | h <- reverse [Hop 1 .. Hop hopRange]
          , let r = table ! (a, xPred, h)
          ]

        hop0 = (xPred, Hop 0)
    in  fromMaybe hop0
        .   asum
        $   (\(h, v, t) -> guard (t <= x) $> (v, h))
        <$> candidates
  else
    let x'     = invert x
        xPred' = invert xPred
        candidates =
          [ (h, fi (r `shiftR` 8), fi r)
          | h <- reverse [Hop 1 .. Hop hopRange]
          , let r = table ! (a, xPred', h)
          ]

        hop0 = (xPred, Hop 0)
    in  fromMaybe hop0
        .   asum
        $   (\(h, v, t) -> guard (t <= x') $> (invert v, negateHop h))
        <$> candidates

invert :: Word8 -> Word8
invert = (255 -)

negateHop :: Hop -> Hop
negateHop (Hop h) = Hop (negate h)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

floatToWord8 :: Float -> Word8
floatToWord8 = round . max 0 . min 255

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
