{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude
import Set1 (hexDecode)
import Data.List (maximum, minimum)

data Maybe a = Nothing | Just a
  deriving (Eq, Ord)

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay s ((x, y):xys)
  | s == x = Just y
  | otherwise = lookupMay s xys

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x y
  | y == 0 = Nothing
  | otherwise = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay l = Just (maximum l)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay l = Just (minimum l)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d s = case lookupMay s d of
  Nothing -> Nothing
  Just xs -> case tailMay xs of
    Nothing -> Nothing
    Just tailOfXs -> case maximumMay tailOfXs of
      Nothing -> Nothing
      Just maximumOfTailOfXs -> case headMay xs of
        Nothing -> Nothing
        Just headOfXs -> divMay (fromIntegral maximumOfTailOfXs) (fromIntegral headOfXs)

testQueryGreek :: (GreekData -> String -> Maybe Double) -> Bool
testQueryGreek queryGreekFn = all (== True)
  [ queryGreekFn greekDataA "alpha" == Just 2.0
  , queryGreekFn greekDataA "beta" == Nothing
  , queryGreekFn greekDataA "gamma" == Just 3.3333333333333335
  , queryGreekFn greekDataA "delta" == Nothing
  , queryGreekFn greekDataA "zeta" == Nothing
  , queryGreekFn greekDataB "rho" == Nothing
  , queryGreekFn greekDataB "phi" == Just 0.24528301886792453
  , queryGreekFn greekDataB "chi" == Just 9.095238095238095
  , queryGreekFn greekDataB "psi" == Nothing
  , queryGreekFn greekDataB "omega" == Just 24.0
  ]

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 = \d s -> link (lookupMay s d)
  $ \xs -> link (tailMay xs)
  $ \tailOfXs -> link (maximumMay tailOfXs)
  $ \maximumOfTailOfXs -> link (headMay xs)
  $ \headOfXs -> divMay (fromIntegral maximumOfTailOfXs) (fromIntegral headOfXs)


addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries t k1 k2 = case lookupMay k1 t of
  Nothing -> Nothing
  Just s1 -> case lookupMay k2 t of
    Nothing -> Nothing
    Just s2 -> mkMaybe (s1 + s2) -- Just (s1 + s2)

-- addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
-- addSalaries = \t k1 k2 -> link (lookupMay k1 t)
--   $ \s1 -> link (lookupMay k2 t)
--   $ \s2 -> Just (s1 + s2)

-- yLink :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
-- yLink _ Nothing  Nothing  = Nothing
-- yLink _ Nothing  _        = Nothing
-- yLink _ _        Nothing  = Nothing
-- yLink f (Just a) (Just b) = f a b

-- Strange, the problem asks for a yLink fn analogous to chain / link
-- above, but (rather than the above defn) turns out they expect the
-- following type signature...

-- Actually (TODO) I think this is supposed to be implemented in terms
-- of link.

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink _ Nothing  Nothing  = Nothing
yLink _ Nothing  _        = Nothing
yLink _ _        Nothing  = Nothing
yLink f (Just a) (Just b) = mkMaybe (f a b) -- Just (f a b)

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 t k1 k2 = yLink (+) (lookupMay k1 t) (lookupMay k2 t)

mkMaybe :: a -> Maybe a
mkMaybe = Just

-- Docs advice, in set 2-5 it is hard to deduce the type signature
-- they expect, especially given
--   yLink :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
-- is more obvious given the defns of chain and link.  Similary, there
-- is no indication for yLink that it should be implemented in terms
-- of link (which then impied in set 4-2).

-- -------
-- Set 2-6
-- -------

-- mfmap :: (a -> b) -> Maybe a -> Maybe b
-- mfmap _ Nothing = Nothing
-- mfmap f (Just a) = Just (f a)

-- tailProd :: Num a => [a] -> Maybe a
-- tailProd = (mfmap product) . tailMay

-- tailProduct :: Num a => [a] -> Maybe a
-- tailProduct xs = case tailMay xs of
--   Nothing -> Nothing
--   Just txs -> Just (product txs)

-- tailSum :: Num a => [a] -> Maybe a
-- tailSum xs = case tailMay xs of
--   Nothing -> Nothing
--   Just txs -> Just (sum txs)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe _ Nothing = Nothing
transMaybe f (Just a) = Just (f a)

tailProd :: Num a => [a] -> Maybe a
tailProd = (transMaybe product) . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = (transMaybe sum) . tailMay

-- tailMax :: Ord a => [a] -> Maybe a
-- tailMax = (transMaybe maximum) . tailMay

-- tailMin :: Ord a => [a] -> Maybe a
-- tailMin = (transMaybe minimum) . tailMay

-- Apparently the type signatures for tailMin and tailMax should
-- actually be:
--   Ord a => [a] -> Maybe (Maybe a)
-- That's because minimum and maximum don't work on empty lists
-- (unlike product and sum above).

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax = (transMaybe maximumMay) . tailMay

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin = (transMaybe minimumMay) . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just m) = m

tailMaxCombined :: Ord a => [a] -> Maybe a
tailMaxCombined = combine . tailMax

tailMinCombined :: Ord a => [a] -> Maybe a
tailMinCombined = combine . tailMin
