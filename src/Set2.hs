{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude
import Numeric (readHex)
import Data.List as List

chunksOf :: Int -> [a] -> [[a]]
chunksOf n l
  | length chunk < n = []
  | otherwise = chunk : chunksOf n rest
    where
      (chunk, rest) = splitAt n l

hexDecode :: String -> String
hexDecode = map (toEnum . fst . head . readHex) . (chunksOf 2)

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
maximumMay l = Just (List.maximum l)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay l = Just (List.minimum l)

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

