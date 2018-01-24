{-# LANGUAGE TypeOperators #-}

-- | Lenses for genesis blockchain types.
--
-- Lenses whose name starts with `genBlock' are from 'GenesisBlock' to
-- small parts of it. It makes it clear what exactly is stored in
-- 'GenesisBlock'. Similar fact is true for `genHeader' prefix.

module Pos.Core.Block.Genesis.Lens
       (
         -- * Extra types
         gehAttributes
       , gebAttributes

         -- * GenesisConsensusData
       , gcdEpoch
       , gcdDifficulty

         -- * GenesisBlockHeader
       , genHeaderPrevBlock
       , genHeaderProof
       , genHeaderEpoch
       , genHeaderDifficulty
       , genHeaderAttributes

         -- * GenesisBody
       , gbLeaders

         -- * GenesisBlock
       , genBlockPrevBlock
       , genBlockProof
       , genBlockEpoch
       , genBlockDifficulty
       , genBlockHeaderAttributes
       , genBlockLeaders
       , genBlockAttributes
       ) where

import           Universum

import           Control.Lens (makeLenses)

import           Pos.Core.Block.Blockchain (gbBody, gbExtra, gbHeader, gbPrevBlock, gbhBodyProof,
                                            gbhConsensus, gbhExtra, gbhPrevBlock)
import           Pos.Core.Block.Genesis.Chain (Body (..), BodyProof (..), ConsensusData (..))
import           Pos.Core.Block.Genesis.Types (GenesisBlock, GenesisBlockHeader, GenesisBlockchain,
                                               GenesisBodyAttributes, GenesisExtraBodyData (..),
                                               GenesisExtraHeaderData (..), GenesisHeaderAttributes)
import           Pos.Core.Common (ChainDifficulty, HeaderHash, SlotLeaders)
import           Pos.Core.Slotting.Types (EpochIndex (..))

----------------------------------------------------------------------------
-- Extra types
----------------------------------------------------------------------------

makeLenses ''GenesisExtraHeaderData
makeLenses ''GenesisExtraBodyData

----------------------------------------------------------------------------
-- ConsensusData
----------------------------------------------------------------------------

makeLenses 'GenesisConsensusData

----------------------------------------------------------------------------
-- GenesisBlockHeader
----------------------------------------------------------------------------

-- | Lens from 'GenesisBlockHeader' to 'HeaderHash' of its parent.
genHeaderPrevBlock :: Lens' (GenesisBlockHeader v) HeaderHash
genHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'GenesisBlockHeader' to 'GenesisProof'.
genHeaderProof :: Lens' (GenesisBlockHeader v) (BodyProof (GenesisBlockchain v))
genHeaderProof = gbhBodyProof

-- | Lens from 'GenesisBlockHeader' to 'EpochIndex'.
genHeaderEpoch :: Lens' (GenesisBlockHeader v) EpochIndex
genHeaderEpoch = gbhConsensus . gcdEpoch

-- | Lens from 'GenesisBlockHeader' to 'ChainDifficulty'.
genHeaderDifficulty :: Lens' (GenesisBlockHeader v) ChainDifficulty
genHeaderDifficulty = gbhConsensus . gcdDifficulty

-- | Lens from 'GenesisBlockHeader' to 'GenesisHeaderAttributes'.
genHeaderAttributes ::
       Lens' (GenesisBlockHeader v) GenesisHeaderAttributes
genHeaderAttributes = gbhExtra . gehAttributes

----------------------------------------------------------------------------
-- GenesisBody
----------------------------------------------------------------------------

makeLenses 'GenesisBody

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

-- | Lens from 'GenesisBlock' to 'HeaderHash' of its parent.
genBlockPrevBlock :: Lens' (GenesisBlock v) HeaderHash
genBlockPrevBlock = gbPrevBlock

-- | Lens from 'GenesisBlock' to 'GenesisProof'.
genBlockProof :: Lens' (GenesisBlock v) (BodyProof (GenesisBlockchain v))
genBlockProof = gbHeader . genHeaderProof

-- | Lens from 'GenesisBlock' to 'EpochIndex'.
genBlockEpoch :: Lens' (GenesisBlock v) EpochIndex
genBlockEpoch = gbHeader . genHeaderEpoch

-- | Lens from 'GenesisBlock' to 'ChainDifficulty'.
genBlockDifficulty :: Lens' (GenesisBlock v) ChainDifficulty
genBlockDifficulty = gbHeader . genHeaderDifficulty

-- | Lens from 'GenesisBlock' to 'GenesisHeaderAttributes'.
genBlockHeaderAttributes ::
       Lens' (GenesisBlock v) GenesisHeaderAttributes
genBlockHeaderAttributes = gbHeader . genHeaderAttributes

-- | Lens from 'GenesisBlock' to 'SlotLeaders'.
genBlockLeaders :: Lens' (GenesisBlock v) SlotLeaders
genBlockLeaders = gbBody . gbLeaders

-- | Lens from 'GenesisBlock' to 'GenesisBodyAttributes'.
genBlockAttributes :: Lens' (GenesisBlock v) GenesisBodyAttributes
genBlockAttributes = gbExtra . gebAttributes
