{-|
Module      : Pager.Units
Description : Defines units of measurements used by pager
Copyright   : (c) Norbert Melzer, 2014
License     : MIT
Maintainer  : timmelzer@gmail.com
Stability   : experimental
Portability : portable

TODO: Longer description follows later!
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Pager.Units where

import           Pager.Internal

-- | represents a length\/size\/value in pt (1/72 of an inch)
newtype Points = Points Int deriving (Show)

instance Unbox Points Int where
  ub (Points x) = x

-- | represents a length\/size\/value in thousands of a meter (see <http://en.wikipedia.org/wiki/International_System_of_Units>)
newtype MilliMeters = MilliMeters Int deriving (Show)

instance Unbox MilliMeters Int where
  ub (MilliMeters x) = x

-- | represents a length\/size\/value in thousands of an inch
newtype MilliInches = MilliInches Int deriving (Show)

instance Unbox MilliInches Int where
  ub (MilliInches x) = x

-- | Converts 'Points' into 'MilliMeters'
--
-- Example:
--
-- >>> pt2mm $ Points 72
-- MilliMeters 25
pt2mm :: Points      -- ^ a value in 'Points'
      -> MilliMeters -- ^ the value in 'MilliMeters'
pt2mm (Points pt) = MilliMeters (round (0.352777778 * fromIntegral pt))

-- | Converts 'Points' into 'MilliInches'
--
-- Example:
--
-- >>> pt2minch $ Points 72
-- MilliInches 1000
pt2minch :: Points      -- ^ a value in 'Points'
         -> MilliInches -- ^ the value in 'MilliInches'
pt2minch (Points pt) = MilliInches (round ((fromIntegral pt / 72) * 1000))

-- | Converts 'MilliMeters' to 'Points'
--
-- Example:
--
-- >>> mm2pt $ MilliMeters 1000
-- Points 2835
mm2pt :: MilliMeters -- ^ a value in 'MilliMeters'
      -> Points      -- ^ the value in 'Points'
mm2pt (MilliMeters mm) = Points (round (2.834645669 * fromIntegral mm))

-- | Converts 'MilliMeters' to 'MilliInches'
--
-- Example:
--
-- >>> mm2minch $ MilliMeters 1000
-- MilliInches 39370
mm2minch :: MilliMeters -- ^ a value in 'MilliMeters'
         -> MilliInches -- ^ the value in 'MilliInches'
mm2minch (MilliMeters mm) = MilliInches (round (39.370079 * fromIntegral mm))

-- | Converts 'MilliInches' to 'Points'
--
-- Example:
--
-- >>> minch2pt $ MilliInches 1000
-- Points 72
minch2pt :: MilliInches -- ^ a value in 'MilliInches'
         -> Points      -- ^ the value in 'Points'
minch2pt (MilliInches minch) = Points (round ((fromIntegral minch * 72) / 1000))

-- | Converts 'MilliInches' to 'MilliMeters'
--
-- Example:
--
-- >>> minch2mm $ MilliInches 1000
-- MilliMeters 25
minch2mm :: MilliInches -- ^ a value in 'MilliInches'
         -> MilliMeters -- ^ the value in 'MilliMeters'
minch2mm (MilliInches minch) = MilliMeters (round (fromIntegral minch * 0.0254))
