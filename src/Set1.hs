{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = [r1, r2, r3, r4, r5]
  where
    s0       = mkSeed 1
    (r1, s1) = rand s0
    (r2, s2) = rand s1
    (r3, s3) = rand s2
    (r4, s4) = rand s3
    (r5, _)  = rand s4

-- randLetter :: Seed -> (Char, Seed)
randLetter :: Gen Char
randLetter seed = (toLetter r, s)
  where (r, s) = rand seed

randString3 :: String
randString3 = [c1, c2, c3]
  where
    s0       = mkSeed 1
    (c1, s1) = randLetter s0
    (c2, s2) = randLetter s1
    (c3, _)  = randLetter s2

-- randEven :: Gen Integer -- the output of rand * 2
-- randEven seed = (r * 2, s)
--   where
--     (r, s) = rand seed

-- randOdd :: Gen Integer -- the output of rand * 2 + 1
-- randOdd seed = (rEven + 1, s)
--   where
--     (rEven, s) = randEven seed

-- randTen :: Gen Integer -- the output of rand * 10
-- randTen seed = (r * 10, s)
--   where
--     (r, s) = rand seed

-- generalA :: (Integer -> a) -> Gen a
-- generalA f seed = (f r, s)
--   where
--     (r, s) = rand seed

-- randEven :: Gen Integer -- the output of rand * 2
-- randEven = generalA (* 2)

-- -- randOdd :: Gen Integer -- the output of rand * 2 + 1
-- -- randOdd = randEven

-- randTen :: Gen Integer -- the output of rand * 10
-- randTen = generalA (* 10)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f genA seed = (f a, s)
  where
    (a, s) = genA seed

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (* 2) rand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA (+ 1) randEven

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (* 10) rand


randPair :: Gen (Char, Integer)
randPair seed = ((c, i), s2)
  where
    (c, s1) = randLetter seed
    (i, s2)  = rand s1

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair genA genB seed = ((a, b), s2)
  where
    (a, s1) = genA seed
    (b, s2) = genB s1

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f genA genB seed = (f a b, s2)
  where
    (a, s1) = genA seed
    (b, s2) = genB s1

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (\a b -> (a, b))

--repRandom :: [Seed -> (a, Seed)] -> Seed -> ([a], Seed)
repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([], s)
repRandom (g:gs) s = ((a:as), sn)
  where
    (a, s1) = g s
    (as, sn) = repRandom gs s1

--genTwo :: (Seed -> (a, Seed)) -> (a -> (Seed -> (b, Seed))) -> Seed -> (b, Seed)
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo genA aToGenB s0 = (b, sn)
  where
    (a, s1) = genA s0
    (b, sn) = aToGenB a s1

mkGen :: a -> Gen a
mkGen a s = (a, s)

-- --repRandom2 :: [Seed -> (a, Seed)] -> Seed -> ([a], Seed)
-- repRandom2 :: [Gen a] -> Gen [a]
-- repRandom2 [] = mkGen []
-- --repRandom2 (g:gs) = genTwo g (\a -> mkGen [a]) -- obvs not right
-- repRandom2 (g:gs) = genTwo g (\a -> repRandom gs) -- obvs not right
