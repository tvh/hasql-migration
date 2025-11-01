-- |
-- Module      : Main
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--               (c) 2016 Timo von Holtz <tvh@tvholtz.de>
--
-- License     : BSD-style
-- Maintainer  : tvh@tvholtz.de
-- Stability   : experimental
-- Portability : GHC
--
-- The test entry-point for hasql-migration.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main
    ( main
    ) where

import Hasql.Connection
#if MIN_VERSION_hasql(1,9,0)
import Hasql.Connection.Setting (connection)
import qualified Hasql.Connection.Setting.Connection as Connection
#endif
import Hasql.MigrationTest
import Test.Hspec (hspec)

main :: IO ()
main = do
    let connstr = "host=localhost port=5432 user=hasql password=hasql dbname=hasql-migration-test"
#if MIN_VERSION_hasql(1,9,0)
    conE <- acquire $ [connection . Connection.string $ connstr]
#else
    conE <- acquire connstr
#endif
    case conE of
      Right con -> hspec (migrationSpec con)
      Left err -> putStrLn $ show err
