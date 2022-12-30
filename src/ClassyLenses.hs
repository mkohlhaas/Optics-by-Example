-- {-# OPTIONS_GHC -ddump-splices #-}

module ClassyLenses where

import Control.Lens
  ( Lens',
    makeClassy,
    makeFields,
    makeLenses,
    view,
    (^.),
  )
import Control.Monad.Reader (MonadIO (..), MonadReader (ask))
import Control.Monad.State (MonadIO (..))
import DB (DatabaseUrl, HasDatenbankUrl (..))
import DBClassy

-----------------------
-- 14. Classy Lenses --
-----------------------

----------------------------------------------------------
-- 14.1 What are Classy Lenses and When Do I Need Them? --
----------------------------------------------------------

-- Classy lenses aren't really a new type of optic.
-- They are a DESIGN PATTERN solving the following issues:
--
-- • Duplicate Record Fields
-- • Separating Logic
-- • Granular Dependencies

--------------------------------
-- No Duplicate Record Fields --
--------------------------------

-- In Haskell we can't have two records with the same field names!
-- Classy lenses provide a solution for this - an imperfect one, though.

-- If we have several records which all have a `name` field, we need to disambiguate the fields.
-- Idiomatically we use the record name as a field prefix.
newtype Persona = Persona
  { _personaName ∷ String
  }
  deriving (Show)

newtype Pet = Pet
  { _petName ∷ String
  }
  deriving (Show)

-- Not only is this annoyingly verbose, but there's a greater problem in Haskell as a whole.
-- We can't write code which is polymorphic over record fields!
-- If we want to print a record's name, we need a separate method for each record type.

greetPerson ∷ Persona → String
greetPerson p = "Hello " <> _personaName p <> "!"

greetPet ∷ Pet → String
greetPet p = "Hello " <> _petName p <> "!"

-- |
-- >>> greetPerson (Persona "Calvin")
-- "Hello Calvin!"

-- |
-- >>> greetPet (Pet "Hobbes")
-- "Hello Hobbes!"

-- We can use `makeFields` to generate record-polymorphic lenses for all the fields which have been defined in the idiomatic way,
-- i.e. with record name as a field prefix.
--
-- name ∷ Lens' Persona String
makeFields ''Persona

-- name ∷ Lens' Pet String
makeFields ''Pet

-- `makeFields` creates a unified `HasName` class and instances for each of the records:

-- >>> :info HasName
-- class HasName s a | s → a where
--   name ∷ Lens' s a
-- instance HasName Persona String
-- instance HasName Pet String

-- :browse
-- class HasName s a | s → a where
--   name ∷ Lens' s a

-- :info HasName
-- instance HasName Person String
-- instance HasName Pet String

-- unifying our two greeting functions
greetByName ∷ HasName a String ⇒ a → String
greetByName a = "Hello " <> a ^. name <> "!"

-- |
-- >>> greetByName (Persona "Calvin")
-- "Hello Calvin!"

-- |
-- >>> greetByName (Pet "Hobbes")
-- "Hello Hobbes!"

------------------------------------------------------
-- Separating Logic and Minimizing Global Knowledge --
------------------------------------------------------

data DataDBEnv where
  DataDBEnv ∷
    { _portNumber ∷ Int,
      _hostName ∷ String,
      _databaseUrl ∷ String
    } →
    DataDBEnv
  deriving (Show)

makeLenses ''DataDBEnv

connectDB ∷ (MonadIO m, MonadReader DataDBEnv m) ⇒ m ()
connectDB = do
  url ← view databaseUrl
  liftIO $ putStrLn ("connecting to db at: " <> url)

initialize ∷ (MonadIO m, MonadReader DataDBEnv m) ⇒ m ()
initialize = do
  port ← view portNumber
  host ← view hostName
  liftIO $ putStrLn ("initializing server at: " <> host <> ":" <> show port)

-- The code is functional, and doesn't have any problems per se.
-- But both `initialize` and `connectDB` depend directly on Env, and presumably so does EVERY action in our entire code-base that pulls config from the environment.
-- This means that we've set a very rigid dependency on our Env type, if that type changes significantly we'll have to fix code across our entire app!

-- Change the environment of connectDB to only the piece it needs (the DatabaseUrl).
-- Then we manually run the reader layer with the correct piece in `main`.
connectDB' ∷ (MonadIO m, MonadReader DatabaseUrl m) ⇒ m ()
connectDB' = do
  url ← ask
  liftIO $ putStrLn ("connecting to db at: " <> url)

---------------------------------------------
-- Granular Dependencies with `makeFields` --
---------------------------------------------

data EnvDb where
  EnvDb ∷
    { _envDbPNumber ∷ Int,
      _envDbHName ∷ String,
      _envDbDatenbankUrl ∷ DatabaseUrl
    } →
    EnvDb
  deriving (Show)

-- Because we imported the DB module, `makeFields` won't define a new HasDatenbankUrl class from the `_envDbDatenbankUrl` field.
makeFields ''EnvDb

--------------------------------
-- Field Requirements Compose --
--------------------------------

-- See app/Init.hs

---------------------------------------
-- 14.2 `makeFields` vs `makeClassy` --
---------------------------------------

data People where
  People ∷
    { _personName ∷ String,
      _favouriteFood ∷ String
    } →
    People
  deriving (Show)

-- makeFieldsNoPrefix ''People
-- generates:
-- class HasPersonName s a | s → a where
--   personName ∷ Lens' s a
-- class HasFavouriteFood s a | s → a where
--   favouriteFood ∷ Lens' s a

makeClassy ''People

-- generates:
-- class HasPeople c where
--   people ∷ Lens' c People
--   favouriteFood ∷ Lens' c String
--   personName ∷ Lens' c String

-- The key difference between these styles is that makeClassy is a bit less granular.
-- It effectively allows you to specify that a type has a Person nested within it somewhere.

-- Refactoring our Database Config example (see app/DBClassy.hs)
newtype EnvClassy = EnvClassy {_envDbConfig ∷ DbConfig} deriving (Show)

-- Instead of using `makeFields` or `makeClassy`, we can go back to our roots with makeLenses.
makeLenses ''EnvClassy

-- Creates the following lens:
-- envDbConfig ∷ Iso' Env DbConfig

-- We have to write an instance for HasDbConfig which specifies where in our record to find the DbConfig.
-- We're just delegating to the lens we generated with `makeLenses`.
instance HasDbConfig EnvClassy where
  dbConfig ∷ Lens' EnvClassy DbConfig
  dbConfig = envDbConfig

-- |
-- >>> let env = EnvClassy (DbConfig "dbAdmin" 100)
-- >>> env ^. databaseUser
-- "dbAdmin"

-- |
-- >>> let env = EnvClassy (DbConfig "dbAdmin" 100)
-- >>> env ^. maxConnections
-- 100

-- |
-- >>> let env = EnvClassy (DbConfig "dbAdmin" 100)
-- >>> env ^. dbConfig
-- DbConfig {_databaseUser = "dbAdmin", _maxConnections = 100}

-- `makeFields` is more helpful for reducing the annoyances of records with shared names and for implementing field-polymorphic functions.
--  Whereas `makeClassy` tends to scale a little better for large projects.
-- `makeClassy` is less granular, but also requires significantly fewer constraints.
