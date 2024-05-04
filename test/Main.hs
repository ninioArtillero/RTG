{-# LANGUAGE InstanceSigs #-}
module Main (main) where

import           Control.Monad                   (liftM4)
import           Sound.RTG.Ritmo.RhythmicPattern
import           Test.QuickCheck

main :: IO ()
main = do
  quickCheck prop_groupAssoc
  quickCheck prop_groupLeftIdentity
  quickCheck prop_groupRightIdentity
  quickCheck prop_groupLeftInverse
  quickCheck prop_groupRightInverse

-- The Arbitrary class overloads the the 'arbitrary' value,
-- which is a like a random generator for the instantiated type.
-- It it used by default by quickCheck, verboseCheck and the like.
-- Note that the type constructor Gen is a monad,
-- so liftM4 is used to lift the Rhythm constructor into it.
instance Arbitrary Rhythmic where
  arbitrary :: Gen Rhythmic
  arbitrary = do
                -- Larger sizes might be needed to be check with either
                -- n <- arbitrary `suchThat` (>= 0) :: Gen Int
                -- n <- chooseInt (0,maxBound)
                n <- chooseInt (0,100)
                s <- arbitrary :: Gen Sign
                let sgn = if n == 0 then Sign 0 else s
                liftM4 Rhythm (vector n) (return []) (return n) (return sgn)

-- This instance lifts into OnsetPattern
instance Arbitrary Binary where
  arbitrary :: Gen Binary
  arbitrary = oneof [return Zero, return One]

instance Arbitrary Sign where
  -- | 0 is not looked for as it is an edge case of meter,
  -- already accounted for on the Rhythmic instance.
  arbitrary :: Gen Sign
  arbitrary = frequency [(2, return $ Sign 1), (1, return $ Sign (-1))]

prop_groupAssoc :: Rhythmic -> Rhythmic -> Rhythmic -> Bool
prop_groupAssoc rhythmA rhythmB rhythmC =
  (rhythmA <> rhythmB) <> rhythmC == rhythmA <> (rhythmB <> rhythmC)

prop_groupLeftIdentity :: Rhythmic -> Bool
prop_groupLeftIdentity rhythm =
  mempty <> rhythm == rhythm

prop_groupRightIdentity :: Rhythmic -> Bool
prop_groupRightIdentity rhythm =
   rhythm <> mempty == rhythm

prop_groupLeftInverse :: Rhythmic -> Bool
prop_groupLeftInverse rhythm =
  inv rhythm <> rhythm == mempty

prop_groupRightInverse :: Rhythmic -> Bool
prop_groupRightInverse rhythm =
  rhythm <> inv rhythm == mempty
