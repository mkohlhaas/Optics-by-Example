{-# LANGUAGE GADTs #-}

module Init (HasHName (..), HasPNumber (..), initialisieren) where

import Control.Lens
import Control.Monad.Reader

data InitFields where
  InitFields ∷
    { _hName ∷ String,
      _pNumber ∷ Int
    } →
    InitFields

makeFieldsNoPrefix ''InitFields

-- A benefit of field typeclasses is that they're specified with constraints, and constraints compose!
-- This means that if we want to accept a record with multiple specific fields we can do so easily by just adding multiple Has* constraints.
initialisieren ∷ (MonadIO m, HasHName e String, HasPNumber e Int, MonadReader e m) ⇒ m ()
initialisieren = do
  port ← view pNumber
  host ← view hName
  liftIO $ putStrLn ("initializing server at: " <> host <> ":" <> show port)
