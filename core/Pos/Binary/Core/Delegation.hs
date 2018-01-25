{-# LANGUAGE DataKinds #-}

-- | Delegation types serialization.

module Pos.Binary.Core.Delegation () where

import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Binary.Core.Slotting ()
import           Pos.Binary.Crypto ()
import           Pos.Core.Delegation (DlgPayload (..), HeavyDlgIndex (..), LightDlgIndices (..))
import           Pos.Util.Verification (Ver (..))

instance Bi (HeavyDlgIndex) where
    encode = encode . getHeavyDlgIndex
    decode = HeavyDlgIndex <$> decode

instance Bi (LightDlgIndices) where
    encode = encode . getLightDlgIndices
    decode = LightDlgIndices <$> decode

instance Bi (DlgPayload 'Unver) where
    encode = encode . getDlgPayload
    decode = UnsafeDlgPayload <$> decode
