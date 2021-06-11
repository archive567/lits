{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Literals
  ( combinations,
    combinationsR,
    toLexiPosR,
    fromLexiPosR,
    binom,
    binomR,
    literals,
    literal,
  )
where

import Data.Bool
import qualified Data.List as List
import Prelude

-- | combinations k xs generates set of k-combinations from xs
--
-- >>> combinations 2 [0..4]
-- [[0,1],[0,2],[0,3],[0,4],[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations m l = [x : ys | x : xs <- List.tails l, ys <- combinations (m - 1) xs]

-- | k-element combinations in reverse lexicographic order.
--
-- >>> combinationsR 2 [0..4]
-- [[3,4],[2,4],[1,4],[0,4],[2,3],[1,3],[0,3],[1,2],[0,2],[0,1]]
combinationsR :: Int -> [a] -> [[a]]
combinationsR 0 _ = [[]]
combinationsR m l = reverse <$> combinations m (reverse l)

-- | Given a combination, what is its position in reverse lexicographic ordering of all combinations.
--
-- https://math.stackexchange.com/questions/1363239/fast-way-to-get-a-position-of-combination-without-repetitions
-- https://math.stackexchange.com/questions/1368526/fast-way-to-get-a-combination-given-its-position-in-reverse-lexicographic-or/1368570#1368570
--
-- >>> toLexiPosR 52 2 [50,51]
-- 0
--
-- >>>  toLexiPosR 52 2 [0,1]
-- 1325
--
-- >>> toLexiPosR 5 2 <$> combinationsR 2 [0..4]
-- [0,1,2,3,4,5,6,7,8,9]
toLexiPosR :: Int -> Int -> [Int] -> Int
toLexiPosR n k xs = binom n k - 1 - sum (zipWith binom xs [1 ..])

-- | Given a reverse lexicographic position, what was the combination?
--
-- >>> (\xs -> xs == fmap (fromLexiPosR 5 2 . toLexiPosR 5 2) xs) (combinations 2 [0..4])
-- True
--
fromLexiPosR :: Int -> Int -> Int -> [Int]
fromLexiPosR n k p = go (n - 1) k ((binom n k - 1) - p) []
  where
    go n' k' p' xs =
      bool
        ( bool
            (go (n' - 1) k' p' xs)
            (go (n' - 1) (k' - 1) (p' - binom n' k') (n' : xs))
            (p' >= binom n' k')
        )
        xs
        (length xs == k)

-- | binomial equation
--
-- The number of 7-card combinations for a 52 card deck is:
--
-- >>> binom 52 7
-- 133784560
binom :: Int -> Int -> Int
binom _ 0 = 1
binom 0 _ = 0
binom n k = product [(n - k + 1) .. n] `div` product [1 .. k]

-- | recursive version of binomial equation
binomR :: Int -> Int -> Int
binomR _ 0 = 1
binomR 0 _ = 0
binomR n k = binomR (n - 1) (k - 1) * n `div` k

-- | The literals
literals :: [String]
literals =
  mconcat $
    fmap List.unwords <$>
    (combinations <$>
     [1 .. 5] <*>
     pure
     (List.words
      "avoid success at all costs"
     ))

-- | Provide the n'th literal assuming lexicographical order.
--
-- >>> literal 0
-- "avoid"
--
-- >> literal 30
-- "avoid success at all costs"
literal :: Int -> String
literal n = literals List.!! n
