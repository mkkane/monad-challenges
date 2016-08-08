{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set1 (hexDecode)
import Set2 ( Maybe(..)
            , headMay
            , tailMay
            , lookupMay
            , divMay
            , maximumMay
            , minimumMay )
import Set3 (Card(..))

-- Set1 general fns:
--
-- generalA :: (a -> b) -> Gen a -> Gen b
-- generalPair :: Gen a -> Gen b -> Gen (a, b)
-- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c

-- Set2 general fns:
--
-- chain :: (a -> Maybe b) -> Maybe a -> Maybe b
-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- transMaybe :: (a -> b) -> Maybe a -> Maybe b
-- combine :: Maybe (Maybe a) -> Maybe a

-- Set1 correspondences with Set2 general fns:
--
-- generalB = yLink
-- genTwo = link

genBYLink :: (a -> b -> c) -> m a -> m b -> m c
genBYLink = undefined

genTwoLink :: m a -> (a -> m b) -> m b
genTwoLink = undefined

-- generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- generalB2 f ga gb = genTwo ga
--   $ \a -> genTwo gb
--   $ \b -> mkGen (f a b)

-- yLink2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- yLink2 f ma mb = link ma
--   $ \a -> link mb
--   $ \b -> mkMaybe (f a b)

-- generalA2 :: (a -> b) -> Gen a -> Gen b
-- generalA2 f ga = genTwo ga (mkGen . f)

-- repRandom2 :: [Gen a] -> Gen [a]
-- repRandom2 [] = mkGen []
-- repRandom2 (ga:gas) = genTwo ga (\a -> generalA2 (a:) (repRandom2 gas))

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

generalB2YLink :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
generalB2YLink f ma mb = bind ma
  $ \a -> bind mb
  $ \b -> return (f a b)

instance Monad Maybe where
  bind Nothing _ = Nothing
  bind (Just a) f = f a
  return = Just

instance Monad [] where
  bind [] _ = []
  bind (a:as) f = f a ++ bind as f
  return a = [a]

-- newtype Gen a = Gen (Seed -> (a, Seed))

-- instance Monad Gen where
--   bind (Gen ga) f = Gen (\s0 -> (let (a, sa) = ga s0 in (let Gen gb = f a in gb sa)))
--   return a = Gen ((,) a) -- Gen (\s -> (a, s))

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Monad Gen where
  bind ga f = Gen (\s0 -> let (a, sa) = runGen ga s0 in runGen (f a) sa)
  return a = Gen ((,) a)

evalGen :: Gen a -> Seed -> a
evalGen (Gen ga) = fst . ga

sequence :: (Monad m) => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) = bind ma (\a -> bind (sequence mas) (\as -> return (a:as)))

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = bind ma
  $ \a -> bind mb
  $ \b -> return (f a b)

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: (Monad m) => m (m a) -> m a
join = (=<<) id
-- join mma = bind mma id

liftM3 :: (Monad m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = bind ma (\a -> liftM2 (f a) mb mc)
-- must be a nicer way to write that (maybe with (=<<) and liftM2)

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = liftM2 (\f -> \a -> f a)

-- Kinda useful to have this too...
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f = (=<<) (return . f)

-- ===================================================================
-- Set1
-- ===================================================================

randInt :: Gen Integer
randInt = Gen rand

fiveRands :: [Integer]
fiveRands = evalGen (sequence (replicate 5 (randInt))) (mkSeed 1)

randLetter :: Gen Char
randLetter = liftM toLetter randInt

randString3 :: String
randString3 = evalGen (sequence (replicate 3 randLetter)) (mkSeed 1)

generalA :: (a -> b) -> Gen a -> Gen b
generalA = liftM

randEven :: Gen Integer
randEven = liftM (* 2) randInt

randOdd :: Gen Integer
randOdd = liftM (+ 1) randEven

randTen :: Gen Integer
randTen = liftM (* 10) randInt

randPair :: Gen (Char, Integer)
randPair = bind randLetter
  $ \c -> bind randInt
  $ \i -> return (c, i)
  -- (better...) liftM2 (,) randLetter randInt

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair = liftM2 (,)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = liftM2

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind

mkGen :: a -> Gen a
mkGen = return

-- ===================================================================
-- Set2
-- ===================================================================

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d s = bind (lookupMay s d)
  $ \xs -> bind (tailMay xs)
  $ \tailOfXs -> bind (maximumMay tailOfXs)
  $ \maximumOfTailOfXs -> bind (headMay xs)
  $ \headOfXs -> divMay (fromIntegral maximumOfTailOfXs) (fromIntegral headOfXs)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain = (=<<)

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = bind

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries t k1 k2 = liftM2 (+) (lookupMay k1 t) (lookupMay k2 t)

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink = liftM2

mkMaybe :: a -> Maybe a
mkMaybe = return

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe = liftM

tailProd :: Num a => [a] -> Maybe a
tailProd = (liftM product) . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = (liftM sum) . tailMay

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax = (liftM maximumMay) . tailMay

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin = (liftM minimumMay) . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine = join

tailMaxCombined :: Ord a => [a] -> Maybe a
tailMaxCombined = join . tailMax

tailMinCombined :: Ord a => [a] -> Maybe a
tailMinCombined = join . tailMin

-- ===================================================================
-- Set3
-- ===================================================================

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = liftM2 (,)

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 Card

combStep :: [a -> b] -> [a] -> [b]
combStep = ap

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = liftM2

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = liftM3
