{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set1 (hexDecode, Gen, genTwo, mkGen, generalA)
import Set2 (Maybe, link, mkMaybe)

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

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = genTwo ga
  $ \a -> genTwo gb
  $ \b -> mkGen (f a b)

yLink2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink2 f ma mb = link ma
  $ \a -> link mb
  $ \b -> mkMaybe (f a b)

generalA2 :: (a -> b) -> Gen a -> Gen b
generalA2 f ga = genTwo ga (mkGen . f)

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 [] = mkGen []
repRandom2 (ga:gas) = genTwo ga (\a -> generalA2 (a:) (repRandom2 gas))

