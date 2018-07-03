{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' instances for 'Pos.Infra.Communication' types defined in 'src'

module Test.Pos.Infra.Arbitrary.Communication () where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Infra.Communication.Types.Relay (DataMsg (..))
import           Pos.Infra.Txp.Network.Types (TxMsgContents (..))

import           Test.Pos.Txp.Arbitrary ()

instance Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
    shrink = genericShrink
