-- {-# OPTIONS_GHC -ddump-splices #-}

-- We export the DBConfig alongside the typeclass since anyone implementing the typeclass
-- will presumably need to have a DBConfig nested in their object somewhere.

module DBClassy (DbConfig (..), HasDbConfig (..), connectDBClassy) where

import Control.Lens
import Control.Monad.Reader

data DbConfig where
  DbConfig ∷
    { _databaseUser ∷ String,
      _maxConnections ∷ Int
    } →
    DbConfig
  deriving (Show)

makeClassy ''DbConfig

-- creates:
--
-- class HasDbConfig c where
--   dbConfig ∷ Lens' c DbConfig
--   databaseUser ∷ Lens' c String
--   maxConnections ∷ Lens' c Int
--   ....
-- instance HasDbConfig DbConfig where
--   dbConfig = id
--   databaseUser f (DbConfig x1 x2) = (fmap (\ y1 → (DbConfig y1) x2)) (f x1)
--   maxConnections f (DbConfig x1 x2) = (fmap (\ y1 → (DbConfig x1) y1)) (f x2)

-- The action now only needs to specify HasDbConfig making no mention of specific fields.
connectDBClassy ∷ (MonadIO m, HasDbConfig e, MonadReader e m) ⇒ m ()
connectDBClassy = do
  dbUser ← view databaseUser
  numConnections ← view maxConnections
  liftIO $ putStrLn ("connecting to db with: " <> dbUser <> " and max connections: " <> show numConnections)
