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

import           Hasql.Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import           Hasql.Transaction (statement, Transaction)
import Data.Text (Text)

-- | Checks if the table with the given name exists in the database.
existsTable :: Text -> Transaction Bool
existsTable table =
    fmap (not . null) $ statement table q
    where
        q = Statement sql (Encoders.param (Encoders.nonNullable Encoders.text)) (Decoders.rowList (Decoders.column (Decoders.nullable Decoders.int8))) False
        sql = "select relname from pg_class where relname = $1"
