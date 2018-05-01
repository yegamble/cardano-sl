-- | Utilities for dealing with 'Buildable'
module Util.Buildable (
    ShowThroughBuild(..)
  ) where

import           Formatting (sformat)
import           Prelude (Show (..))
import           Test.QuickCheck (Arbitrary (..))
import           Universum

newtype ShowThroughBuild a = STB { unSTB :: a }
  deriving (Eq, Ord)

instance Buildable a => Show (ShowThroughBuild a) where
  show = toString . sformat F.build . unSTB

instance Arbitrary a => Arbitrary (ShowThroughBuild a) where
  arbitrary = STB <$> arbitrary
