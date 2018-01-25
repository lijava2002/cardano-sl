{-# LANGUAGE TypeOperators #-}

-- | Miscellaneous instances, etc. Related to the main blockchain of course.

module Pos.Core.Block.Main.Instances
       (
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, stext, (%))
import           Serokell.Util (Color (Magenta), colorize, listJson)

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core.Block ()
import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..))
import           Pos.Core.Block.Main.Chain (Body (..), ConsensusData (..))
import           Pos.Core.Block.Main.Lens (mainBlockBlockVersion, mainBlockDifficulty,
                                           mainBlockSlot, mainBlockSoftwareVersion,
                                           mainHeaderBlockVersion, mainHeaderDifficulty,
                                           mainHeaderLeaderKey, mainHeaderSlot,
                                           mainHeaderSoftwareVersion, mbTxs, mcdDifficulty,
                                           mehBlockVersion, mehSoftwareVersion)
import           Pos.Core.Block.Main.Types (MainBlock, MainBlockHeader, MainBlockchain,
                                            MainExtraHeaderData (..))
import           Pos.Core.Block.Union.Types (BlockHeader, blockHeaderHash)
import           Pos.Core.Class (HasBlockVersion (..), HasDifficulty (..), HasEpochIndex (..),
                                 HasEpochOrSlot (..), HasHeaderHash (..), HasSoftwareVersion (..),
                                 IsHeader, IsMainHeader (..))
import           Pos.Core.Common (HeaderHash)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Slotting.Types (EpochOrSlot (..), slotIdF)
import           Pos.Crypto (hashHexF)


instance Bi (BlockHeader v) => Buildable (MainBlockHeader v) where
    build gbh@UnsafeGenericBlockHeader {..} =
        bprint
            ("MainBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    slot: "%slotIdF%"\n"%
             "    difficulty: "%int%"\n"%
             "    leader: "%build%"\n"%
             "    signature: "%build%"\n"%
             build
            )
            gbhHeaderHash
            _gbhPrevBlock
            _mcdSlot
            _mcdDifficulty
            _mcdLeaderKey
            _mcdSignature
            _gbhExtra
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ Right gbh
        MainConsensusData {..} = _gbhConsensus

instance (HasConfiguration, Bi (BlockHeader v)) => Buildable (MainBlock v) where
    build UnsafeGenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             "  transactions ("%int%" items): "%listJson%"\n"%
             "  "%build%"\n"%
             "  "%build%"\n"%
             "  update payload: "%build%"\n"%
             "  "%build
            )
            (colorize Magenta "MainBlock")
            _gbHeader
            (length txs)
            txs
            _mbDlgPayload
            _mbSscPayload
            _mbUpdatePayload
            _gbExtra
      where
        MainBody {..} = _gbBody
        txs = _gbBody ^. mbTxs

instance HasEpochIndex (MainBlock v) where
    epochIndexL = mainBlockSlot . epochIndexL

instance HasEpochIndex (MainBlockHeader v) where
    epochIndexL = mainHeaderSlot . epochIndexL

instance HasEpochOrSlot (MainBlockHeader v) where
    getEpochOrSlot = EpochOrSlot . Right . view mainHeaderSlot

instance HasEpochOrSlot (MainBlock v) where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

-- NB. it's not a mistake that these instances require @Bi BlockHeader@
-- instead of @Bi MainBlockHeader@. We compute header's hash by
-- converting it to a BlockHeader first.

instance Bi (BlockHeader v) =>
         HasHeaderHash (MainBlockHeader v) where
    headerHash = blockHeaderHash . Right

instance Bi (BlockHeader v) =>
         HasHeaderHash (MainBlock v) where
    headerHash = blockHeaderHash . Right . _gbHeader

instance HasDifficulty (ConsensusData (MainBlockchain v)) where
    difficultyL = mcdDifficulty

instance HasDifficulty (MainBlockHeader v) where
    difficultyL = mainHeaderDifficulty

instance HasDifficulty (MainBlock v) where
    difficultyL = mainBlockDifficulty

instance HasBlockVersion MainExtraHeaderData where
    blockVersionL = mehBlockVersion

instance HasSoftwareVersion MainExtraHeaderData where
    softwareVersionL = mehSoftwareVersion

instance HasBlockVersion (MainBlock v) where
    blockVersionL = mainBlockBlockVersion

instance HasSoftwareVersion (MainBlock v) where
    softwareVersionL = mainBlockSoftwareVersion

instance HasBlockVersion (MainBlockHeader v) where
    blockVersionL = mainHeaderBlockVersion

instance HasSoftwareVersion (MainBlockHeader v) where
    softwareVersionL = mainHeaderSoftwareVersion

instance Bi (BlockHeader v) => IsHeader (MainBlockHeader v)

instance Bi (BlockHeader v) => IsMainHeader (MainBlockHeader v) where
    headerSlotL = mainHeaderSlot
    headerLeaderKeyL = mainHeaderLeaderKey
