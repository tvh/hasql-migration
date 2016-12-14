-- |
-- Module      : Main
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- The test entry-point for postgresql-simple-migration.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Hasql.Connection
import Hasql.MigrationTest
import           Test.Hspec                               (hspec)

main :: IO ()
main = do
    con <- acquire "dbname=test"
    case con of
      Right con -> hspec (migrationSpec con)
      Left err -> putStrLn $ show err
