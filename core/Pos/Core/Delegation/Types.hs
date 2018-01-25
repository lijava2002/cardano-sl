{-# LANGUAGE DataKinds #-}

-- | Core delegation types.
module Pos.Core.Delegation.Types
       (
         LightDlgIndices (..)
       , ProxySigLight
       , ProxySKLight

       , HeavyDlgIndex (..)
       , ProxySigHeavy
       , ProxySKHeavy

       , DlgPayload (..)
       , DlgProof
       , mkDlgProof
       ) where

import           Universum

import           Data.Coerce (coerce)
import           Data.Default (Default (def))
import qualified Data.Text.Buildable
import           Formatting (bprint, build, int, (%))
import           Serokell.Util (listJson, pairF)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Slotting.Types (EpochIndex)
import           Pos.Crypto (Hash, ProxySecretKey (..), ProxySignature, unsafeHash)
import           Pos.Util.Verification (Ver (..))

----------------------------------------------------------------------------
-- Proxy signatures and signing keys
----------------------------------------------------------------------------

-- Notice: light delegation was removed as part of CSL-1856 and should
-- be reworked later. Though some parts of it are left to support
-- backward compatibility.

-- | Pair of indices for light delegation PSK that define start and
-- end epoch of cert usage. Block is valid if its epoch index is
-- inside this range.
data LightDlgIndices =
    LightDlgIndices { getLightDlgIndices :: (EpochIndex, EpochIndex) }
    deriving (Show, Eq, Generic)

instance NFData LightDlgIndices

instance Buildable LightDlgIndices where
    build (LightDlgIndices p) = bprint pairF p

-- | Light delegation proxy signature, that holds a pair of epoch
-- indices.
type ProxySigLight v a = ProxySignature v LightDlgIndices a

-- | Same alias for the proxy secret key (see 'ProxySigLight').
type ProxySKLight v = ProxySecretKey v LightDlgIndices


-- | Witness for heavy delegation signature -- epoch in which
-- certificate starts being active. It is needed for replay attack
-- prevention (index should match epoch of the block PSK is announced
-- in).
data HeavyDlgIndex =
    HeavyDlgIndex { getHeavyDlgIndex :: EpochIndex }
    deriving (Show, Eq, Generic)

instance NFData HeavyDlgIndex

instance Buildable HeavyDlgIndex where
    build (HeavyDlgIndex i) = bprint build i

-- | Simple proxy signature without ttl/epoch index constraints.
type ProxySigHeavy v a = ProxySignature v HeavyDlgIndex a

-- | Heavy delegation PSK.
type ProxySKHeavy v = ProxySecretKey v HeavyDlgIndex

----------------------------------------------------------------------------
-- Payload
----------------------------------------------------------------------------

-- Consider making this a set.
-- | 'DlgPayload' is put into 'MainBlock' and consists of a list of
-- heavyweight proxy signing keys. There must be no duplicates
-- (comparing by issuer) in this list. The order of PSKs doesn't
-- matter, as it's checked for cycles after bulk application. So it's
-- technically a set.
newtype DlgPayload v = UnsafeDlgPayload
    { getDlgPayload :: [ProxySKHeavy v]
    } deriving (Show, Eq, Generic, NFData)

instance Default (DlgPayload v) where
    def = UnsafeDlgPayload []

instance Buildable (DlgPayload v) where
    build (UnsafeDlgPayload psks) =
        bprint
            ("proxy signing keys ("%int%" items): "%listJson%"\n")
            (length psks) psks

-- | Proof of delegation payload.
type DlgProof = Hash (DlgPayload 'Ver)

-- | Creates 'DlgProof' out of delegation payload.
mkDlgProof :: (Bi (DlgPayload 'Unver)) => DlgPayload v -> DlgProof
mkDlgProof p = unsafeHash (coerce p :: DlgPayload 'Unver)
