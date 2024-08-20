{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import           Control.Monad                   (unless)
import           Sound.RTG.Ritmo.RhythmicPattern
import           System.Exit                     (exitFailure)
import           Test.QuickCheck

main :: IO ()
main = do
  let tests = [
        quickCheckResult prop_groupAssoc,
        quickCheckResult prop_groupLeftIdentity,
        quickCheckResult prop_groupRightIdentity,
        quickCheckResult prop_groupLeftInverse,
        quickCheckResult prop_groupRightInverse
        ]
  success <- fmap (all isSuccess) . sequence $ tests
  unless success exitFailure

-- The Arbitrary class overloads the the 'arbitrary' value,
-- which is a like a random generator for the instantiated type.
-- It it used by default by quickCheck, verboseCheck and the like.
-- Note that the type constructor Gen is a monad,
-- so fmap is used to lift the Rhythm constructor into it.
instance Arbitrary (Rhythm Binary) where
  arbitrary :: Gen (Rhythm Binary)
  arbitrary = do
                -- Larger sizes might be needed to be check with either
                -- n <- arbitrary `suchThat` (>= 0) :: Gen Int
                -- n <- chooseInt (0,maxBound)
                n <- chooseInt (0,100)
                fmap Rhythm (vector n)

instance Arbitrary Binary where
  arbitrary :: Gen Binary
  arbitrary = oneof [return Zero, return One]

-- instance Arbitrary Sign where
--   -- | 0 is not generated as it is an edge case of meter,
--   -- already accounted for on the Rhythmic instance.
--   arbitrary :: Gen Sign
--   arbitrary = frequency [(2, return $ Sign 1), (1, return $ Sign (-1))]

prop_groupAssoc :: Rhythm Binary -> Rhythm Binary -> Rhythm Binary -> Bool
prop_groupAssoc rhythmA rhythmB rhythmC =
  (rhythmA <> rhythmB) <> rhythmC == rhythmA <> (rhythmB <> rhythmC)

prop_groupLeftIdentity :: Rhythm Binary -> Bool
prop_groupLeftIdentity rhythm =
  mempty <> rhythm == rhythm

prop_groupRightIdentity :: Rhythm Binary -> Bool
prop_groupRightIdentity rhythm =
   rhythm <> mempty == rhythm

prop_groupLeftInverse :: Rhythm Binary -> Bool
prop_groupLeftInverse rhythm =
  inv rhythm <> rhythm == mempty

prop_groupRightInverse :: Rhythm Binary -> Bool
prop_groupRightInverse rhythm =
  rhythm <> inv rhythm == mempty
