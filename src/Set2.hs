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

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

--queryGreek2 :: GreekData -> String -> Maybe Double
--queryGreek2 d s = (\x -> \y -> divMay (fromIntegral x) (fromIntegral y)) (chain headMay $ lookupMay s d) (chain maximumMay $ chain tailMay $ lookupMay s d)

-- div $ (fromIntegral . maximum . tail . lookup) s d (fromIntegral . head . lookup) s d

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 = undefined
