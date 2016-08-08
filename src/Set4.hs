{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set1 (hexDecode)
-- import Set1 (hexDecode, Gen, genTwo, mkGen, generalA)
import Set2

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
