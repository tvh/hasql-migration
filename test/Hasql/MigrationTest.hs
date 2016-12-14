-- |
-- Module      : Database.PostgreSQL.Simple.MigrationTest
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--               (c) 2016 Timo von Holtz <tvh@tvholtz.de>
--
-- License     : BSD-style
-- Maintainer  : tvh@tvholtz.de
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of hasql-migration specifications.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasql.MigrationTest where

import           Hasql.Session                        (run, Error)
import           Hasql.Connection
import qualified Hasql.Transaction                    as Tx
import           Hasql.Migration
import           Hasql.Migration.Util                 (existsTable)
import           Test.Hspec                           (Spec, describe, it,
                                                       shouldBe, runIO)

runTx :: Connection -> Tx.Transaction a -> IO (Either Error a)
runTx con act = do
    run (Tx.run act Tx.ReadCommitted Tx.Write) con

migrationSpec :: Connection -> Spec
migrationSpec con = describe "Migrations" $ do
    let migrationScript = MigrationScript "test.sql" q
    let migrationScriptAltered = MigrationScript "test.sql" ""
    [migrationDir] <- runIO $ loadMigrationsFromDirectory "share/test/scripts"
    migrationFile <- runIO $ loadMigrationFromFile "s.sql" "share/test/script.sql"

    it "initializes a database" $ do
        r <- runTx con $ runMigration $ MigrationInitialization
        r `shouldBe` Right MigrationSuccess

    it "creates the schema_migrations table" $ do
        r <- runTx con $ existsTable "schema_migration"
        r `shouldBe` Right True

    it "executes a migration script" $ do
        r <- runTx con $ runMigration $ migrationScript
        r `shouldBe` Right MigrationSuccess

    it "creates the table from the executed script" $ do
        r <- runTx con $ existsTable "t1"
        r `shouldBe` Right True

    it "skips execution of the same migration script" $ do
        r <- runTx con $ runMigration $ migrationScript
        r `shouldBe` Right MigrationSuccess

    it "reports an error on a different checksum for the same script" $ do
        r <- runTx con $ runMigration $ migrationScriptAltered
        r `shouldBe` Right (MigrationError "test.sql")

    it "executes migration scripts inside a folder" $ do
        r <- runTx con $ runMigration $ migrationDir
        r `shouldBe` Right MigrationSuccess

    it "creates the table from the executed scripts" $ do
        r <- runTx con $ existsTable "t2"
        r `shouldBe` Right True

    it "executes a file based migration script" $ do
        r <- runTx con $ runMigration $ migrationFile
        r `shouldBe` Right MigrationSuccess

    it "creates the table from the executed scripts" $ do
        r <- runTx con $ existsTable "t3"
        r `shouldBe` Right True

    it "validates initialization" $ do
        r <- runTx con $ runMigration $ (MigrationValidation MigrationInitialization)
        r `shouldBe` Right MigrationSuccess

    it "validates an executed migration script" $ do
        r <- runTx con $ runMigration $ (MigrationValidation migrationScript)
        r `shouldBe` Right MigrationSuccess

    it "validates all scripts inside a folder" $ do
        r <- runTx con $ runMigration $ (MigrationValidation migrationDir)
        r `shouldBe` Right MigrationSuccess

    it "validates an executed migration file" $ do
        r <- runTx con $ runMigration $ (MigrationValidation migrationFile)
        r `shouldBe` Right MigrationSuccess

    it "gets a list of executed migrations" $ do
        r <- runTx con getMigrations
        fmap (map schemaMigrationName) r `shouldBe` Right ["test.sql", "1.sql", "s.sql"]

    where
        q = "create table t1 (c1 varchar);"

