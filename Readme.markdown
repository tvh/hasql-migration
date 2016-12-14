# PostgreSQL Migrations for Haskell

[![Build Status](https://api.travis-ci.org/tvh/hasql-migration.png)](https://travis-ci.org/tvh/hasql-migration)

Welcome to hasql-migrations, a tool for helping you with
PostgreSQL schema migrations. This is a port of
[postgresql-simple-migration](https://github.com/ameingast/postgresql-simple-migration)
for use with hasql.

## Why?
Database migrations should not be hard. They should be under version control
and documented in both your production systems and in your project files.

## What?
This library executes SQL/Haskell migration scripts and keeps track of their
meta information.

Scripts are be executed exactly once and any changes to scripts will cause
a run-time error notifying you of a corrupted database.

The meta information consists of:
* an MD5 checksum of the executed script to make sure already existing
  scripts cannot be modified in your production system.
* a time-stamp of the date of execution so you can easily track when a change
  happened.

This library also supports migration validation so you can ensure (some)
correctness before your application logic kicks in.

## How?
TODO

## Compilation and Tests
The program is built with the _cabal_ build system. The following command
builds the library, the standalone binary and the test package.

```bash
cabal configure --enable-tests && cabal build -j
```

To execute the tests, you need a running PostgreSQL server with an empty
database called _test_. Tests are executed through cabal as follows:

```bash
cabal configure --enable-tests && cabal test
```

To build the project in a cabal sandbox, use the following code:

```bash
cabal sandbox init
cabal install -j --only-dependencies --enable-tests --disable-documentation
cabal configure --enable-tests
cabal test
```

To remove the generated cabal sandbox, use:
```bash
cabal sandbox delete
```

## To Do
* Collect executed scripts and check if already executed scripts have been
  deleted.
