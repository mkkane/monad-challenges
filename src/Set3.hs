{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude
import Set1 (hexDecode)

-- allPairs :: [a] -> [b] -> [(a, b)]
-- allPairs [] _ = []
-- allPairs _ [] = []
-- allPairs (x:xs) ys = map ((,) x) ys ++ allPairs xs ys

-- allPairsAlt :: [a] -> [b] -> [(a, b)]
-- allPairsAlt [] _ = []
-- allPairsAlt _ [] = []
-- allPairsAlt (x:xs) (y:ys) = (x, y) : allPairsAlt [x] ys ++ allPairsAlt xs (y:ys)

data Card = Card Int String

instance Show Card where
  show (Card rank suit) = (show rank) ++ suit

-- allCards :: [Int] -> [String] -> [Card]
-- allCards ranks suits = map (\(r, s) -> Card r s) (allPairs ranks suits)

-- allCards :: [Int] -> [String] -> [Card]
-- allCards [] _ = []
-- allCards _ [] = []
-- allCards (r:ranks) suits = map (\s -> Card r s) suits ++ allCards ranks suits

-- allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
-- allCombs _ [] _ = []
-- allCombs _ _ [] = []
-- allCombs f (x:xs) ys = map (f x) ys ++ allCombs f xs ys

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = allCombs (,)

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

-- allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- allCombs3 _ [] _ _ = []
-- allCombs3 f (x:xs) ys zs = map (\(y, z) -> f x y z) (allCombs (,) ys zs)
--   ++ allCombs3 f xs ys zs

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep _ [] = []
combStep (f:fs) as = map f as ++ combStep fs as

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs = combStep (map f as) bs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = combStep (combStep (map f as) bs) cs
