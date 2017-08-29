-- |
-- Module      : Hasql..Migration
-- Copyright   : (c) 2016 Timo von Holtz <tvh@tvholtz.de>,
--               (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : tvh@tvholtz.de
-- Stability   : experimental
-- Portability : GHC
--
-- A migration library for hasql.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hasql.Migration
    (
    -- * Migration actions
    runMigration
    , loadMigrationFromFile
    , loadMigrationsFromDirectory

    -- * Migration types
    , MigrationCommand(..)
    , MigrationError(..)
    , ScriptName
    , Checksum

    -- * Migration result actions
    , getMigrations

    -- * Migration result types
    , SchemaMigration(..)
    ) where

import Control.Arrow
import Crypto.Hash (hashWith, MD5(..))
import Data.ByteArray.Encoding
import Data.Default.Class
import Data.Functor.Contravariant
import Data.List (isPrefixOf, sort)
import Data.Time (LocalTime)
import Data.Traversable (forM)
import Hasql.Migration.Util (existsTable)
import Hasql.Query
import Hasql.Transaction
import System.Directory (getDirectoryContents)
import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

-- | Executes a 'MigrationCommand'.
--
-- Returns 'MigrationSuccess' if the provided 'MigrationCommand' executes
-- without error. If an error occurs, execution is stopped and
-- a 'MigrationError' is returned.
runMigration :: MigrationCommand -> Transaction (Maybe MigrationError)
runMigration cmd = case cmd of
    MigrationInitialization ->
        initializeSchema >> return Nothing
    MigrationScript name contents ->
        executeMigration name contents
    MigrationValidation validationCmd ->
        executeValidation validationCmd

-- | Load migrations from SQL scripts in the provided 'FilePath'
-- in alphabetical order.
loadMigrationsFromDirectory :: FilePath -> IO [MigrationCommand]
loadMigrationsFromDirectory dir = do
    scripts <- scriptsInDirectory dir
    forM scripts $ \f -> loadMigrationFromFile f (dir ++ "/" ++ f)

-- | load a migration from script located at the provided
-- 'FilePath'.
loadMigrationFromFile :: ScriptName -> FilePath -> IO MigrationCommand
loadMigrationFromFile name fp =
    MigrationScript name <$> BS.readFile fp


-- | Lists all files in the given 'FilePath' 'dir' in alphabetical order.
scriptsInDirectory :: FilePath -> IO [String]
scriptsInDirectory dir =
    fmap (sort . filter (\x -> not $ "." `isPrefixOf` x))
        (getDirectoryContents dir)
-- | Executes a generic SQL migration for the provided script 'name' with
-- content 'contents'.
executeMigration :: ScriptName -> BS.ByteString -> Transaction (Maybe MigrationError)
executeMigration name contents = do
    let checksum = md5Hash contents
    checkScript name checksum >>= \case
        ScriptOk -> do
            return Nothing
        ScriptNotExecuted -> do
            sql contents
            query (name, checksum) (statement q (contramap (first T.pack) def) Decoders.unit False)
            return Nothing
        ScriptModified _ -> do
            return (Just $ ScriptChanged name)
    where
        q = "insert into schema_migrations(filename, checksum) values($1, $2)"

-- | Initializes the database schema with a helper table containing
-- meta-information about executed migrations.
initializeSchema :: Transaction ()
initializeSchema = do
    sql $ mconcat
        [ "create table if not exists schema_migrations "
        , "( filename varchar(512) not null"
        , ", checksum varchar(32) not null"
        , ", executed_at timestamp without time zone not null default now() "
        , ");"
        ]

-- | Validates a 'MigrationCommand'. Validation is defined as follows for these
-- types:
--
-- * 'MigrationInitialization': validate the presence of the meta-information
-- table.
-- * 'MigrationValidation': always succeeds.
executeValidation :: MigrationCommand -> Transaction (Maybe MigrationError)
executeValidation cmd = case cmd of
    MigrationInitialization ->
        existsTable "schema_migrations" >>= \r -> return $ if r
            then Nothing
            else (Just NotInitialised)
    MigrationScript name contents ->
        validate name contents
    MigrationValidation _ ->
        return Nothing
    where
        validate name contents =
            checkScript name (md5Hash contents) >>= \case
                ScriptOk -> do
                    return Nothing
                ScriptNotExecuted -> do
                    return (Just $ ScriptMissing name)
                ScriptModified _ -> do
                    return (Just $ ChecksumMismatch name)

-- | Checks the status of the script with the given name 'name'.
-- If the script has already been executed, the checksum of the script
-- is compared against the one that was executed.
-- If there is no matching script entry in the database, the script
-- will be executed and its meta-information will be recorded.
checkScript :: ScriptName -> Checksum -> Transaction CheckScriptResult
checkScript name checksum =
    query name (statement q (contramap T.pack (Encoders.value def)) (Decoders.maybeRow (Decoders.value def)) False) >>= \case
        Nothing ->
            return ScriptNotExecuted
        Just actualChecksum | checksum == actualChecksum ->
            return ScriptOk
        Just actualChecksum ->
            return (ScriptModified actualChecksum)
    where
        q = mconcat
            [ "select checksum from schema_migrations "
            , "where filename = $1 limit 1"
            ]

-- | Calculates the MD5 checksum of the provided bytestring in base64
-- encoding.
md5Hash :: BS.ByteString -> Checksum
md5Hash = T.decodeUtf8 . convertToBase Base64 . hashWith MD5

-- | The checksum type of a migration script.
type Checksum = T.Text

-- | The name of a script. Typically the filename or a custom name
-- when using Haskell migrations.
type ScriptName = String

-- | 'MigrationCommand' determines the action of the 'runMigration' script.
data MigrationCommand
    = MigrationInitialization
    -- ^ Initializes the database with a helper table containing meta
    -- information.
    | MigrationScript ScriptName BS.ByteString
    -- ^ Executes a migration based on the provided bytestring.
    | MigrationValidation MigrationCommand
    -- ^ Validates the provided MigrationCommand.
    deriving (Show, Eq, Read, Ord)

-- | A sum-type denoting the result of a single migration.
data CheckScriptResult
    = ScriptOk
    -- ^ The script has already been executed and the checksums match.
    -- This is good.
    | ScriptModified Checksum
    -- ^ The script has already been executed and there is a checksum
    -- mismatch. This is bad.
    | ScriptNotExecuted
    -- ^ The script has not been executed, yet. This is good.
    deriving (Show, Eq, Read, Ord)

-- | Errors that could occur when a migration is validated or performed
data MigrationError = ScriptChanged String | NotInitialised | ScriptMissing String | ChecksumMismatch String deriving (Show, Eq, Read, Ord)

-- | Produces a list of all executed 'SchemaMigration's.
getMigrations :: Transaction [SchemaMigration]
getMigrations =
    query () $ statement q def (Decoders.rowsList decodeSchemaMigration) False
    where
        q = mconcat
            [ "select filename, checksum, executed_at "
            , "from schema_migrations order by executed_at asc"
            ]

-- | A product type representing a single, executed 'SchemaMigration'.
data SchemaMigration = SchemaMigration
    { schemaMigrationName       :: BS.ByteString
    -- ^ The name of the executed migration.
    , schemaMigrationChecksum   :: Checksum
    -- ^ The calculated MD5 checksum of the executed script.
    , schemaMigrationExecutedAt :: LocalTime
    -- ^ A timestamp without timezone of the date of execution of the script.
    } deriving (Show, Eq, Read)

instance Ord SchemaMigration where
    compare (SchemaMigration nameLeft _ _) (SchemaMigration nameRight _ _) =
        compare nameLeft nameRight

decodeSchemaMigration :: Decoders.Row SchemaMigration
decodeSchemaMigration =
    SchemaMigration
    <$> Decoders.value def
    <*> Decoders.value def
    <*> Decoders.value def
