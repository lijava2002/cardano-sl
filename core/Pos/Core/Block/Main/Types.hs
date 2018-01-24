{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

-- | Types defining the main blockchain.

module Pos.Core.Block.Main.Types
       ( MainBlockchain
       , MainBlockHeader
       , MainExtraBodyData (..)
       , MainExtraHeaderData (..)
       , BlockHeaderAttributes
       , BlockBodyAttributes
       , BlockSignature (..)
       , MainToSign (..)
       , MainBlock
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, builder, (%))

import           Pos.Binary.Crypto ()
import           Pos.Core.Block.Blockchain (Blockchain (..), GenericBlock (..),
                                            GenericBlockHeader (..))
import           Pos.Core.Common (ChainDifficulty, HeaderHash)
import           Pos.Core.Delegation (ProxySigHeavy, ProxySigLight)
import           Pos.Core.Slotting.Types (SlotId (..))
import           Pos.Core.Update.Types (BlockVersion, SoftwareVersion)
import           Pos.Crypto (Hash, Signature)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)
import           Pos.Util.Verification (Ver (..))

-- | Represents blockchain consisting of main blocks, i. e. blocks
-- with actual payload (transactions, SSC, update system, etc.).
data MainBlockchain (v :: Ver)

-- | Data to be signed in main block.
data MainToSign v
    = MainToSign
    { _msHeaderHash  :: !HeaderHash  -- ^ Hash of previous header
                                     --    in the chain
    , _msBodyProof   :: !(BodyProof (MainBlockchain v))
    , _msSlot        :: !SlotId
    , _msChainDiff   :: !ChainDifficulty
    , _msExtraHeader :: !MainExtraHeaderData
    } deriving Generic

-- | Signature of the block. Can be either regular signature from the
-- issuer or delegated signature having a constraint on epoch indices
-- (it means the signature is valid only if block's slot id has epoch
-- inside the constrained interval).
data BlockSignature (v :: Ver)
    = BlockSignature (Signature (MainToSign v))
    | BlockPSignatureLight (ProxySigLight v (MainToSign v))
    | BlockPSignatureHeavy (ProxySigHeavy v (MainToSign v))
    deriving (Show, Eq, Generic)

instance NFData (BodyProof (MainBlockchain v)) => NFData (BlockSignature v)

instance Buildable (BlockSignature v) where
    build (BlockSignature s)       = bprint ("BlockSignature: "%build) s
    build (BlockPSignatureLight s) = bprint ("BlockPSignatureLight: "%build) s
    build (BlockPSignatureHeavy s) = bprint ("BlockPSignatureHeavy: "%build) s

-- | Represents main block body attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending block with new
-- fields via softfork.
type BlockBodyAttributes = Attributes ()

-- | Represents main block header attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending header with new
-- fields via softfork.
type BlockHeaderAttributes = Attributes ()

-- | Represents main block header extra data
data MainExtraHeaderData = MainExtraHeaderData
    { -- | Version of block.
      _mehBlockVersion    :: !BlockVersion
    , -- | Software version.
      _mehSoftwareVersion :: !SoftwareVersion
    , -- | Header attributes
      _mehAttributes      :: !BlockHeaderAttributes
    , -- | Extra body data Hash
      _mehEBDataProof     :: !(Hash MainExtraBodyData)
    } deriving (Eq, Show, Generic)

instance NFData MainExtraHeaderData

instance Buildable MainExtraHeaderData where
    build MainExtraHeaderData {..} =
      bprint ( "    block: v"%build%"\n"
             % "    software: "%build%"\n"
             % builder
             )
            _mehBlockVersion
            _mehSoftwareVersion
            formattedExtra
      where
        formattedExtra
            | areAttributesKnown _mehAttributes = mempty
            | otherwise = bprint ("    attributes: "%build%"\n") _mehAttributes

-- | Represents main block extra data
newtype MainExtraBodyData = MainExtraBodyData
    { _mebAttributes  :: BlockBodyAttributes
    } deriving (Eq, Show, Generic, NFData)

instance Buildable MainExtraBodyData where
    build (MainExtraBodyData attrs)
        | areAttributesKnown attrs = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs

-- | Header of generic main block.
type MainBlockHeader v = GenericBlockHeader (MainBlockchain v)

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock v = GenericBlock (MainBlockchain v)
