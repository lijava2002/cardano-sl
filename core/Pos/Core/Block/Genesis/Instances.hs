{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Miscellaneous instances, etc. Related to the genesis blockchain of course.

module Pos.Core.Block.Genesis.Instances
       (
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, sformat, stext, (%))
import           Serokell.Util (Color (Magenta), colorize, listJson)

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core.Block ()
import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..), gbHeader,
                                            gbhConsensus)
import           Pos.Core.Block.Genesis.Chain (Body (..), ConsensusData (..))
import           Pos.Core.Block.Genesis.Lens (gcdDifficulty, gcdEpoch)
import           Pos.Core.Block.Genesis.Types (GenesisBlock, GenesisBlockHeader, GenesisBlockchain)
import           Pos.Core.Block.Union.Types (BlockHeader, blockHeaderHash)
import           Pos.Core.Class (HasDifficulty (..), HasEpochIndex (..), HasEpochOrSlot (..),
                                 HasHeaderHash (..), IsGenesisHeader, IsHeader)
import           Pos.Core.Common (HeaderHash)
import           Pos.Core.Slotting.Types (EpochOrSlot (..))
import           Pos.Crypto (hashHexF)
import           Pos.Util.Verification (Ver (..))

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance Bi (BlockHeader v) => Buildable (GenesisBlockHeader v) where
    build gbh@UnsafeGenericBlockHeader {..} =
        bprint
            ("GenesisBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    epoch: "%build%"\n"%
             "    difficulty: "%int%"\n"
            )
            gbhHeaderHash
            _gbhPrevBlock
            _gcdEpoch
            _gcdDifficulty
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ Left gbh
        GenesisConsensusData {..} = _gbhConsensus

instance Bi (BlockHeader v) => Buildable (GenesisBlock v) where
    build UnsafeGenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             stext
            )
            (colorize Magenta "GenesisBlock")
            _gbHeader
            formatLeaders
      where
        GenesisBody {..} = _gbBody
        formatIfNotNull formatter l = if null l then mempty else sformat formatter l
        formatLeaders = formatIfNotNull
            ("  leaders: "%listJson%"\n") _gbLeaders

----------------------------------------------------------------------------
-- Pos.Core.Class
----------------------------------------------------------------------------

instance HasEpochIndex (GenesisBlock v) where
    epochIndexL = gbHeader . gbhConsensus . gcdEpoch

instance HasEpochIndex (GenesisBlockHeader v) where
    epochIndexL = gbhConsensus . gcdEpoch

instance HasEpochOrSlot (GenesisBlockHeader v) where
    getEpochOrSlot = EpochOrSlot . Left . _gcdEpoch . _gbhConsensus

instance HasEpochOrSlot (GenesisBlock v) where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

-- NB. it's not a mistake that these instances require @Bi BlockHeader@
-- instead of @Bi GenesisBlockHeader@. We compute header's hash by
-- converting it to a BlockHeader first.

instance Bi (BlockHeader v) =>
         HasHeaderHash (GenesisBlockHeader v) where
    headerHash = blockHeaderHash . Left

instance Bi (BlockHeader v) =>
         HasHeaderHash (GenesisBlock v) where
    headerHash = blockHeaderHash . Left . _gbHeader

instance HasDifficulty (ConsensusData (GenesisBlockchain v)) where
    difficultyL = gcdDifficulty

instance HasDifficulty (GenesisBlockHeader v) where
    difficultyL = gbhConsensus . difficultyL

instance HasDifficulty (GenesisBlock v) where
    difficultyL = gbHeader . difficultyL

instance Bi (BlockHeader v) => IsHeader (GenesisBlockHeader v)
instance Bi (BlockHeader v) => IsGenesisHeader (GenesisBlockHeader v)
