{-# LANGUAGE DataKinds #-}

-- | Delegation helpers.

module Pos.Core.Delegation.Util
       ( checkDlgPayload
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.List (groupBy)

import           Pos.Core.Delegation.Types (DlgPayload (..))
import           Pos.Crypto (ProxySecretKey (..))
import           Pos.Util.Verification (Ver (..))

-- | Verifier of 'DlgPayload' which ensures absence of duplicates, or invalid
-- PSKs.
checkDlgPayload :: (MonadError Text m) => DlgPayload 'Ver -> m (DlgPayload 'Ver)
checkDlgPayload it = do
    unless (null duplicates) $
        throwError "Some of block's PSKs have the same issuer, which is prohibited"
    pure it
  where
    proxySKs = getDlgPayload it
    proxySKsDups psks =
        filter (\x -> length x > 1) $
        groupBy ((==) `on` pskIssuerPk) $ sortOn pskIssuerPk psks
    duplicates = proxySKsDups proxySKs
