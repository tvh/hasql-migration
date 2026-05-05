-- |
-- Module      : Database.PostgreSQL.Simple.MigrationTest
-- Copyright   : (c) 2016 Timo von Holtz <tvh@tvholtz.de>,
--               (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : tvh@tvholtz.de
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of hasql-migration specifications.

{-# LANGUAGE OverloadedStrings #-}

module Hasql.MigrationTest where

import           Hasql.Session                        (run, SessionError)
import           Hasql.Connection
import qualified Hasql.Transaction                    as Tx
import qualified Hasql.Transaction.Sessions           as Tx
import           Hasql.Migration
import           Hasql.Migration.Util                 (existsTable)
import           Test.Hspec                           (Spec, describe, it,
                                                       shouldBe, runIO, expectationFailure)
import           Hasql.Statement                      (Statement)
import qualified Hasql.Session                        as Session

runTx :: Connection -> Tx.Transaction a -> IO (Either SessionError a)
runTx con act = do
    run (Tx.transaction Tx.ReadCommitted Tx.Write act) con

runStatement :: Connection -> a -> Statement a b -> IO (Either SessionError b)
runStatement con a stmt = do
  run (Session.statement a stmt) con

spec :: Connection -> Spec
spec con =
  describe "Migration" $ do
    describe "runMigration" $ migrationSpec con

migrationSpec :: Connection -> Spec
migrationSpec con = describe "runMigration" $ do
    let migrationScript = MigrationScript "test.sql" q
    let migrationScriptAltered = MigrationScript "test.sql" ""
    mds <- runIO $ loadMigrationsFromDirectory "share/test/scripts"
    let migrationDir = head mds
    migrationFile <- runIO $ loadMigrationFromFile "s.sql" "share/test/script.sql"

    it "initializes a database" $ do
        r <- runTx con $ runMigration $ MigrationInitialization
        r `shouldBe` Right Nothing

    it "creates the schema_migrations table" $ do
        r <- runStatement con  "schema_migrations" existsTable
        r `shouldBe` Right True

    it "executes a migration script" $ do
        r <- runTx con $ runMigration $ migrationScript
        r `shouldBe` Right Nothing

    it "creates the table from the executed script" $ do
        r <- runStatement con "t1" existsTable
        r `shouldBe` Right True

    it "skips execution of the same migration script" $ do
        r <- runTx con $ runMigration $ migrationScript
        r `shouldBe` Right Nothing

    it "reports an error on a different checksum for the same script" $ do
        r <- runTx con $ runMigration $ migrationScriptAltered
        r `shouldBe` Right (Just (ScriptChanged "test.sql"))

    it "executes migration scripts inside a folder" $ do
        r <- runTx con $ runMigration $ migrationDir
        r `shouldBe` Right Nothing

    it "creates the table from the executed scripts" $ do
        r <- runStatement con "t2" existsTable
        r `shouldBe` Right True

    it "executes a file based migration script" $ do
        r <- runTx con $ runMigration $ migrationFile
        r `shouldBe` Right Nothing

    it "creates the table from the executed scripts" $ do
        r <- runStatement con "t3" existsTable
        r `shouldBe` Right True

    it "validates initialization" $ do
        r <- runTx con $ runMigration $ (MigrationValidation MigrationInitialization)
        r `shouldBe` Right Nothing

    it "validates an executed migration script" $ do
        r <- runTx con $ runMigration $ (MigrationValidation migrationScript)
        r `shouldBe` Right Nothing

    it "validates all scripts inside a folder" $ do
        r <- runTx con $ runMigration $ (MigrationValidation migrationDir)
        r `shouldBe` Right Nothing

    it "validates an executed migration file" $ do
        r <- runTx con $ runMigration $ (MigrationValidation migrationFile)
        r `shouldBe` Right Nothing

    it "creates an index concurrently without using transactions" $ do
      let script = (MigrationScript "no-transaction.sql" "create index concurrently t1_c1_idx on t1(c1)")
      rFail <- runTx con $ runMigration script
      case rFail of
        Right res -> expectationFailure $ "Expected an error but got: " <> show res
        Left _ -> do
          r <- flip run con $ runMigrationWithoutTransactions script
          r `shouldBe` Right Nothing

    it "gets a list of executed migrations" $ do
        r <- runTx con getMigrations
        fmap (map schemaMigrationName) r `shouldBe` Right ["test.sql", "1.sql", "s.sql", "no-transaction.sql"]

    where
        q = "create table t1 (c1 varchar);"
