-- We export the DBConfig alongside the typeclass since anyone implementing the typeclass
-- will presumably need to have a DBConfig nested in their object somewhere.
module DBClassy (DbConfig (..), HasDbConfig (..), connectDBClassy) where

import Control.Lens
import Control.Monad.Reader

data DbConfig = DbConfig
  { _databaseUser ∷ String,
    _maxConnections ∷ Int
  }
  deriving (Show)

makeClassy ''DbConfig

-- The action now only needs to specify HasDbConfig making no mention of specific fields.
connectDBClassy ∷ (MonadIO m, HasDbConfig e, MonadReader e m) ⇒ m ()
connectDBClassy = do
  dbUser ← view databaseUser
  numConnections ← view maxConnections
  liftIO $ putStrLn ("connecting to db with: " <> dbUser <> " and max connections: " <> show numConnections)
