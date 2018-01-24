{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

-- | This module is dedicated to support the verification granularity proposal.
-- See docs/proposals/serialization.md.

module Pos.Util.Verification
    ( Ver(..)
    , VerM
    , verMFail
    , verMToEither
    , verMToFail
    , verMField
    ) where

import           Universum

import qualified Data.Text as T

import           Pos.Util.Util (eitherToThrow)

-- | Verification datatype. For now we support only two levels of data
-- verification.
data Ver = Ver | Unver

newtype VerM c = VerM (Either [Text] c) deriving (Show,Eq,Functor,Applicative,Monad)

data VerError = VerError Text deriving (Show)

instance Exception VerError

verMFail :: Text -> VerM c
verMFail t = VerM (Left [t])

verMToEither :: VerM c -> Either VerError c
verMToEither (VerM x) = first (VerError . T.intercalate "." . reverse) x

verMToFail :: (MonadThrow m) => VerM c -> m c
verMToFail = eitherToThrow . verMToEither

-- | Verifies some field, prefixing with the text value in case of
-- error. Prefix is supposed to be the record field name.
verMField :: Text -> VerM c -> VerM c
verMField p (VerM v) = VerM $ first (\x -> p:x) v
