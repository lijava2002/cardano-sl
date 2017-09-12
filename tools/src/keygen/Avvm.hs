-- | Former avvm-migration script. Parses rscoin-dump to create
-- genesis block of CSL.

module Avvm
       ( AvvmData (..)
       , AvvmEntry (..)
       , avvmAddrDistribution
       , applyBlacklisted
       ) where

import           Universum

import           Control.Exception.Safe (displayException, throwString)
import           Data.Aeson             (FromJSON (..), withObject, (.:))
import qualified Data.ByteString        as BS
import qualified Data.HashMap.Strict    as HM
import           Data.List              ((\\))
import qualified Data.Text              as T
import qualified Serokell.Util.Base64   as B64
import           System.Wlog            (WithLogger, logInfo)

import           Pos.Crypto             (RedeemPublicKey (..), redeemPkBuild)
import           Pos.Genesis            (GenesisAvvmBalances (..))
import           Pos.Types              (Coin, unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Util.Util          (eitherToFail)


-- | Read the text into a redeeming public key.
--
-- There's also a copy of this function in cardano-addr-convert.
fromAvvmPk :: (MonadThrow m, Monad m) => Text -> m RedeemPublicKey
fromAvvmPk addrText = do
    let base64rify = T.replace "-" "+" . T.replace "_" "/"
    let parsedM = B64.decode $ base64rify addrText
    addrParsed <-
        maybe (throwString $ "Address " <> toString addrText <> " is not base64(url) format")
        pure
        (rightToMaybe parsedM)
    unless (BS.length addrParsed == 32) $
        throwString "Address' length is not equal to 32, can't be redeeming pk"
    pure $ redeemPkBuild addrParsed

data AvvmData = AvvmData
    { utxo :: [AvvmEntry]
    } deriving (Show, Generic)

instance FromJSON AvvmData

{- I will use rscoin's dump-state(-new) format for now which doesn't use colored coins,
data AvvmCoin = AvvmCoin
    { coinAmount :: Integer
    , coinColor  :: Integer
    } deriving (Show, Generic)

instance FromJSON AvvmCoin where
    parseJSON = withObject "coin" $ \o -> do
        coinAmount <- o .: "coinAmount"
        coinColor <- o .: "coinColor" >>= (.: "getColor")
        return AvvmCoin{..}
-}

data AvvmEntry = AvvmEntry
    { aeCoin      :: !Integer         -- in lovelaces
    , aePublicKey :: !RedeemPublicKey -- in base64(u), yep
    } deriving (Show, Generic, Eq)

instance FromJSON AvvmEntry where
    parseJSON =
        withObject "avvmEntry" $ \o -> do
            aeCoin <- (* (1000000 :: Integer)) <$> o .: "coin"
            (addrText :: Text) <- o .: "address"
            aePublicKey <-
                eitherToFail $ first displayException $ fromAvvmPk addrText
            return AvvmEntry {..}

-- | Generate genesis address distribution out of avvm
-- parameters. Txdistr of the utxo is all empty. Redelegate it in
-- calling funciton.
avvmAddrDistribution
    :: AvvmData
    -> GenesisAvvmBalances
avvmAddrDistribution (utxo -> avvmData) = GenesisAvvmBalances balances
  where
    balances :: HashMap RedeemPublicKey Coin
    balances =
        HM.fromListWith unsafeAddCoin $ do
            AvvmEntry {..} <- avvmData
            let adaCoin = unsafeIntegerToCoin aeCoin
            return (aePublicKey, adaCoin)

-- | Applies blacklist to avvm utxo, produces warnings and stats about
-- how much was deleted.
applyBlacklisted ::
       (WithLogger m, MonadIO m, MonadThrow m)
    => Maybe FilePath
    -> AvvmData
    -> m AvvmData
applyBlacklisted Nothing r = r <$ logInfo "Blacklisting: file not specified, skipping"
applyBlacklisted (Just blacklistPath) AvvmData{..} = do
    addrTexts <- lines <$> readFile blacklistPath
    blacklisted <- mapM fromAvvmPk addrTexts
    let filteredBad = filter ((`elem` blacklisted) . aePublicKey) utxo
    let filtered = utxo \\ filteredBad
    logInfo $
        "Removing " <> show (length filteredBad) <> " entries from utxo (out of " <>
        show (length blacklisted) <> " total entries in the blacklist)"
    pure $ AvvmData filtered