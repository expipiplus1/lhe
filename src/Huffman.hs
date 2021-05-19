module Huffman where

import           Control.Monad.ST
import           Data.Bifunctor                 ( second )
import           Data.Foldable
import           Data.List                      ( intercalate )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.PQueue.Prio.Min          as PQ
import           Data.STRef.Strict
import qualified Data.Vector.Unboxed           as VU
                                                ( freeze
                                                , toList
                                                )
import qualified Data.Vector.Unboxed.Mutable   as VU

data Bit = Zero | One

instance Show Bit where
  show Zero = "0"
  show One  = "1"

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving Show

type Code a = [(a, [Bit])]

huffman :: (Num p, Ord p) => [(p, a)] -> Code a
huffman =
  let go pq = case PQ.size pq of
        0 -> error "Empty histogram"
        1 -> snd . findMin $ pq
        _ ->
          let ([(p1, a1), (p2, a2)], pq') = PQ.splitAt 2 pq
          in  go $ PQ.insert (p1 + p2) (Branch a1 a2) pq'
      code = \case
        Leaf a -> [(a, [])]
        Branch l r ->
          (second (Zero :) <$> code l) <> (second (One :) <$> code r)
  in  code . go . fromList . fmap (fmap Leaf)

histogram :: Ord a => [a] -> [(Int, a)]
histogram xs = runST $ do
  hRef <- newSTRef =<< VU.new 0
  mRef <- newSTRef Map.empty
  kRef <- newSTRef []
  for_ xs $ \a -> do
    m <- readSTRef mRef
    i <- case Map.lookup a m of
      Just i  -> pure i
      Nothing -> do
        modifySTRef' kRef (a :)
        let i = Map.size m
        modifySTRef' mRef (Map.insert a i)
        h  <- readSTRef hRef
        h' <- VU.grow h 1
        writeSTRef hRef h'
        pure i
    h <- readSTRef hRef
    VU.modify h succ i
  k <- readSTRef kRef
  h <- readSTRef hRef
  flip zip (reverse k) . VU.toList <$> VU.freeze h

-- Pretty-print a binary code.  Mostly useful for debugging.
ppCode :: Show a => Code a -> String
ppCode =
  intercalate "\n" . fmap (\(x, bits) -> show x ++ ": " ++ concatMap show bits)

encode :: Eq a => Code a -> [a] -> [Bit]
encode c = concatMap (fromJust . flip lookup c)
