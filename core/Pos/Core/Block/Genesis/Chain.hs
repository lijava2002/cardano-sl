{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Definitions of the genesis blockchain ('Blockchain' class and related).

module Pos.Core.Block.Genesis.Chain
       ( BodyProof (..)
       , ConsensusData (..)
       , Body (..)
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable

import           Pos.Core.Block.Blockchain (Blockchain (..), BlockchainHelpers (..))
import           Pos.Core.Block.Genesis.Types (GenesisBlock, GenesisBlockchain,
                                               GenesisExtraBodyData, GenesisExtraHeaderData)
import           Pos.Core.Block.Union.Types (Block, BlockHeader)
import           Pos.Core.Common (ChainDifficulty, SlotLeaders)
import           Pos.Core.Slotting.Types (EpochIndex (..))
import           Pos.Crypto (Hash, hash)

instance Blockchain (GenesisBlockchain v) where
    -- [CSL-199]: maybe we should use ADS.
    -- | Proof of GenesisBody is just a hash of slot leaders list.
    data BodyProof (GenesisBlockchain v) = GenesisProof
        !(Hash SlotLeaders)
        deriving (Eq, Generic, Show)
    data ConsensusData (GenesisBlockchain v) = GenesisConsensusData
        { -- | Index of the slot for which this genesis block is relevant.
          _gcdEpoch :: !EpochIndex
        , -- | Difficulty of the chain ending in this genesis block.
          _gcdDifficulty :: !ChainDifficulty
        } deriving (Generic, Show, Eq)
    type BBlockHeader (GenesisBlockchain v) = BlockHeader v
    type ExtraHeaderData (GenesisBlockchain v) = GenesisExtraHeaderData

    -- | Body of genesis block consists of slot leaders for epoch
    -- associated with this block.
    data Body (GenesisBlockchain v) = GenesisBody
        { _gbLeaders :: !SlotLeaders
        } deriving (Generic, Show, Eq)

    type ExtraBodyData (GenesisBlockchain v) = GenesisExtraBodyData
    type BBlock (GenesisBlockchain v) = Block v

    mkBodyProof = GenesisProof . hash . _gbLeaders

instance Buildable (BodyProof (GenesisBlockchain v)) where
    build (GenesisProof h) = Buildable.build h

instance BlockchainHelpers (GenesisBlockchain v) where
    verifyBBlockHeader _ = pure ()
    verifyBBlock _ = pure ()

instance NFData (BodyProof (GenesisBlockchain v))
instance NFData (ConsensusData (GenesisBlockchain v))
instance NFData (Body (GenesisBlockchain v))
instance NFData (GenesisBlock v)
