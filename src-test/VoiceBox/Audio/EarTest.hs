{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module VoiceBox.Audio.EarTest (tests) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import VoiceBox.Audio.Ear

-- import other modules as needed

-- Newtype for well-formed maps of vectors (all vectors same nonzero length, at least 1 key in each map)
newtype WellFormedMapsOfVectors = WellFormedMapsOfVectors (Map.Map Int (Map.Map Int (V.Vector (Int, Int, Int))))
  deriving (Show, Eq)

-- | Checks well-formedness: all vectors have same nonzero length, each map has at least one key.
isWellFormedMapOfVectors :: Map.Map Int (Map.Map Int (V.Vector a)) -> Bool
isWellFormedMapOfVectors m =
  not (Map.null m)
    && not (any Map.null (Map.elems m))
    && let lens = [V.length v | sub <- Map.elems m, v <- Map.elems sub]
        in not (null lens) && all (> 0) lens && all (== head lens) lens

-- | Arbitrary instance for WellFormedMapsOfVectors.
-- Ensures all generated maps are well-formed: non-empty, all vectors same nonzero length, at least one key per map.
instance Arbitrary WellFormedMapsOfVectors where
  arbitrary = sized $ \n -> do
    let topSize = (n `div` 2) `max` 1
        innerSize = max 1 (n `div` (3 * topSize))
        vecLen = max 1 (n `div` (4 * topSize * innerSize))
        genTuple = (,,) <$> arbitrary <*> arbitrary <*> arbitrary
        genVec = V.replicateM vecLen genTuple
        genInnerMap = Map.fromList <$> mapM (\k2 -> (,) k2 <$> genVec) [0 .. innerSize]
    genTopMap <- Map.fromList <$> mapM (\k1 -> (,) k1 <$> genInnerMap) [1 .. topSize]
    pure (WellFormedMapsOfVectors genTopMap)

  -- Only shrink well-formed maps.
  -- Shrink by removing top-level keys (but keep at least one).
  -- Shrink by removing inner keys (but keep at least one per submap).
  -- Shrink by shortening vectors (but keep length > 0 and all equal).
  -- \| Shrink each inner map by removing one key if possible.
  -- \| Remove one key from a submap if it has more than one key.
  shrink (WellFormedMapsOfVectors m)
    | not (isWellFormedMapOfVectors m) = []
    | otherwise =
        -- Shrink by removing top-level keys (but keep at least one)
        [ WellFormedMapsOfVectors m'
        | Map.size m > 1,
          k <- Map.keys m,
          let m' = Map.delete k m,
          isWellFormedMapOfVectors m'
        ]
          ++
          -- Shrink by removing inner keys (but keep at least one per submap)
          [ WellFormedMapsOfVectors m'
          | any (\sub -> Map.size sub > 1) (Map.elems m),
            let m' = shrinkInnerKeys m,
            isWellFormedMapOfVectors m'
          ]
          ++
          -- Shrink by shortening vectors (but keep length > 0 and all equal)
          case [V.length vec | sub <- Map.elems m, vec <- Map.elems sub] of
            lengths@(l : _)
              | l > 1 ->
                  [ WellFormedMapsOfVectors (Map.map (Map.map (V.slice 0 (l - 1))) m)
                  | all (> 0) lengths,
                    all (== l) lengths,
                    isWellFormedMapOfVectors (Map.map (Map.map (V.slice 0 (l - 1))) m)
                  ]
            _ -> []
    where
      shrinkInnerKeys :: Map.Map Int (Map.Map Int (V.Vector a)) -> Map.Map Int (Map.Map Int (V.Vector a))
      shrinkInnerKeys = Map.mapMaybe shrinkSub

      shrinkSub :: Map.Map Int (V.Vector a) -> Maybe (Map.Map Int (V.Vector a))
      shrinkSub sub
        | Map.size sub > 1 = Just (Map.delete (fst (Map.findMin sub)) sub)
        | otherwise = Just sub

-- Property: For well-formed maps, transposer returns Just
prop_unzipMapOfVectors_wellformed_just :: WellFormedMapsOfVectors -> Property
prop_unzipMapOfVectors_wellformed_just (WellFormedMapsOfVectors m) =
  case unzipMapOfVectors m of
    Just _ -> property True
    Nothing -> counterexample "Expected Just, got Nothing" False

-- Property: For malformed maps, transposer returns Nothing
prop_unzipMapOfVectors_malformed_nothing :: Map.Map Int (Map.Map Int (V.Vector (Int, Int, Int))) -> Property
prop_unzipMapOfVectors_malformed_nothing m =
  not (isWellFormedMapOfVectors m) ==>
    case unzipMapOfVectors m of
      Nothing -> property True
      Just _ -> counterexample "Expected Nothing, got Just" False

-- Used int the Unit/Regression test.
wellformedMapOfVectors :: Map.Map Int (Map.Map Int (V.Vector (Int, Int, Int)))
wellformedMapOfVectors =
  Map.fromList $ do
    k1 <- [1 .. 4]
    let subMap =
          Map.fromList $
            [(k2, V.generate 10 (,k2,k1)) | k2 <- [0 .. k1]]
    pure (k1, subMap)

wellformedVectorOfMaps :: V.Vector (Map.Map Int (Map.Map Int (Int, Int, Int)))
wellformedVectorOfMaps = V.generate 10 mkMaps
  where
    mkMaps i =
      Map.fromList
        [(k1, subMap i k1) | k1 <- [1 .. 4]]
    subMap i k1 =
      Map.fromList $
        [(k2, (i, k2, k1)) | k2 <- [0 .. k1]]

-- | Helper: Get the shape of a nested map (outer keys mapped to sets of inner keys).
getMapShape :: Map Int (Map.Map Int a) -> Map Int (Set.Set Int)
getMapShape = Map.map Map.keysSet

-- | Property: Checks both shape and vector length after roundtrip.
prop_unzipMapOfVectors_shape :: WellFormedMapsOfVectors -> Property
prop_unzipMapOfVectors_shape (WellFormedMapsOfVectors m) =
  case unzipMapOfVectors m of
    Nothing -> property True
    Just v ->
      let expectedShape = getMapShape m
          mAtZero = v V.!? 0
          shapeCheck = case mAtZero of
            Nothing -> property True
            Just atZero ->
              counterexample
                "map shape unchanged after roundtrip"
                (getMapShape atZero == expectedShape)
          lenCheck =
            counterexample
              "vector length property failed"
              ( V.length v
                  == ( let lengths = [V.length vec | sub <- Map.elems m, vec <- Map.elems sub]
                        in if null lengths then 0 else head lengths
                     )
              )
       in conjoin [shapeCheck, lenCheck]

-- | Arbitrary instance for Vector
instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink v = V.fromList <$> shrink (V.toList v)

tests :: TestTree
tests =
  testGroup
    "Ear tests"
    [ testCase "wrapPhase" $ do
        assertEqual "wrapPhase 0" 0.0 (wrapPhase 0)
        assertEqual "wrapPhase pi" pi (wrapPhase pi)
        assertEqual "wrapPhase 2pi" 0.0 (wrapPhase (2 * pi))
        assertEqual "wrapPhase -pi" pi (wrapPhase (-pi)),
      test_linearPhaseIntensity,
      testCase "unzipMapOfVectors (unit)" $ do
        let expected = Just wellformedVectorOfMaps
            actual = unzipMapOfVectors wellformedMapOfVectors
        assertEqual "unzipMapOfVectors" expected actual,
      testProperty "unzipMapOfVectors roundtrip" prop_unzipMapOfVectors_shape,
      testProperty "unzipMapOfVectors wellformed returns Just" prop_unzipMapOfVectors_wellformed_just,
      testProperty "unzipMapOfVectors malformed returns Nothing" prop_unzipMapOfVectors_malformed_nothing
    ]

test_linearPhaseIntensity :: TestTree
test_linearPhaseIntensity =
  testGroup
    "linearPhaseIntensity properties"
    [ testProperty "Output is always in [0, 1]" prop_linearPhaseIntensity_range,
      testProperty "0 maps to 0" prop_linearPhaseIntensity_zero,
      testProperty "pi and -pi map to 1" prop_linearPhaseIntensity_pi,
      testProperty "0.5*pi maps to 0.5" prop_linearPhaseIntensity_halfpi,
      testProperty "Values outside [-pi, pi] wrap correctly" prop_linearPhaseIntensity_wrap,
      testCase "linearPhaseIntensity" $ do
        assertEqual "in-phase" 0.0 (linearPhaseIntensity 0)
        assertEqual "out-of-phase pos" 1.0 (linearPhaseIntensity pi)
        assertEqual "out-of-phase neg" 1.0 (linearPhaseIntensity (-pi))
        assertBool "half-phase" (abs (linearPhaseIntensity (pi / 2) - 0.5) < 1e-6)
    ]

--------------------------------------------
-- linearPhaseIntensity Tests --------------
--------------------------------------------

-- Property: Output is always in [0, 1]
prop_linearPhaseIntensity_range :: Double -> Property
prop_linearPhaseIntensity_range x =
  let y = linearPhaseIntensity x
   in counterexample
        ("Got: " ++ show y ++ " for input " ++ show x)
        (y >= 0 && y <= 1)

-- Property: Values outside [-pi, pi] wrap correctly
prop_linearPhaseIntensity_wrap :: Double -> Property
prop_linearPhaseIntensity_wrap x =
  let wrapped = wrapPhase x
   in counterexample
        ( "Input: "
            ++ show x
            ++ ", wrapped: "
            ++ show wrapped
            ++ ", f(x): "
            ++ show (linearPhaseIntensity x)
            ++ ", f(wrapped): "
            ++ show (linearPhaseIntensity wrapped)
        )
        (linearPhaseIntensity x == linearPhaseIntensity wrapped)

-- -- | output is always in [-pi, pi]
-- prop_wrapPhase_range x =

-- -- | wrapPhase is idempotent
-- prop_wrapPhase_idempotent x =

-- -- | Periodicity: wrapPhase (x + 2pi) == wrapPhase x
-- prop_wrapPhase_periodicity x =

--------------------------------------------
-- wrapPhase Tests --------------
--------------------------------------------

-- | output is always in [-pi, pi]
prop_wrapPhase_range :: Double -> Bool
prop_wrapPhase_range x =
  let y = wrapPhase x
   in y >= (-pi) && y <= pi

-- | wrapPhase is idempotent
prop_wrapPhase_idempotent :: Double -> Bool
prop_wrapPhase_idempotent x =
  let once = wrapPhase x
      twice = wrapPhase once
   in once == twice -- this should actually be strictly equal

-- | Periodicity: wrapPhase (x + 2pi) == wrapPhase x
prop_wrapPhase_periodicity :: Double -> Bool
prop_wrapPhase_periodicity x =
  let plus2pi = wrapPhase (x + 2 * pi)
      original = wrapPhase x
   in plus2pi ~~ original

---------------------------------------------
-- helper functions -------------------------
---------------------------------------------

infix 4 ~~

(~~) :: (Floating a, Ord a) => a -> a -> Bool
a ~~ b = abs (a - b) < 1e-6
