-- {-# OPTIONS_GHC -ddump-splices #-}

module Main where

import ClassyLenses
import Control.Lens (Magnify(magnify))
import Control.Monad.Reader
import Control.Monad.State
import DB
import DBClassy
import Data.Map (Map)
import qualified Data.Map as M
import Folds
import IndexableStructures
import IndexedOptics
import Init
import Isos
import Lenses
import Operators
import Optics
import OpticsAndMonads
import PolymorphicOptics
import Prisms
import Traversals

--  Table of Contents (TOC)
--
--  1. Obligatory Preamble
--  2. Optics
--  3. Lenses
--  4. Polymorphic Optics
--  5. Operators
--  6. Folds
--  7. Traversals
--  8. Indexable Structures
--  9. Prisms
-- 10. Isos
-- 11. Indexed Optics
-- 12. Dealing with Type Errors
-- 13. Optics and Monads
-- 14. Classy Lenses
-- 15. JSON
-- 16. Uniplate - Manipulating recursive data
-- 17. generic-lens
-- 18. Appendices
-- 19. Answers to Exercises
-- 20. Thanks
-- Notes

-- TODO: Chapters 16, 17
-- https://github.com/chiroptical/optics-by-example

main ∷ IO ()
main = do
  runReaderT printUser (Env "jenkins" (M.singleton "jenkins" "hunter2"))
  runReaderT printUser' (Env "jenkins" (M.singleton "jenkins" "hunter2"))
  runReaderT getUserPassword (Env "jenkins" (M.singleton "jenkins" "hunter2"))
  runReaderT getUserPassword (Env "jenkins" (M.singleton "jenkin" "hunter2"))
  -- Using connectDB.
  flip runReaderT (DataDBEnv 8000 "example.com" "db.example.com") $ do
    initialize
    connectDB
  -- Trying to use connectDB'.
  -- This is messy, and also requires you to know your full monad stack.
  -- This is a definite anti-pattern in MTL style and not a viable option!
  flip runReaderT (DataDBEnv 8000 "example.com" "db.example.com") $ do
    initialize
    ask >>= \env → runReaderT connectDB' (_databaseUrl env)
  -- `magnify` can help a little with this situation.
  flip runReaderT (DataDBEnv 8000 "example.com" "db.example.com") $ do
    initialize
    magnify databaseUrl connectDB'
  -- Using `makeFields` cleans up this mess. See next paragraph for details of `EnvDB` and app/DB.hs, app/Init.hs files.
  flip runReaderT (EnvDb 8000 "example.com" "db.example.com") $ do
    -- TODO
    -- initialisieren
    connectDatenbank
