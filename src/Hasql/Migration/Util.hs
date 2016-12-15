-- |
-- Module      : Hasql.Migration.Util
-- Copyright   : (c) 2016 Timo von Holtz <tvh@tvholtz.de>,
--               (c) 2014-2016 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : tvh@tvholtz.de
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of utilites for database migrations.

{-# LANGUAGE OverloadedStrings #-}

module Hasql.Migration.Util
    ( existsTable
    ) where

import           Hasql.Query
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import           Hasql.Transaction (query, Transaction)
import Data.Text (Text)
import Data.Default.Class

-- | Checks if the table with the given name exists in the database.
existsTable :: Text -> Transaction Bool
existsTable table =
    fmap (not . null) $ query table q
    where
        q = statement sql (Encoders.value def) (Decoders.rowsList (Decoders.value Decoders.int8)) False
        sql = "select count(relname) from pg_class where relname = $1"
