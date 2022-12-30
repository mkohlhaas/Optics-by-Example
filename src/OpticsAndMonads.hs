-- {-# OPTIONS_GHC -ddump-splices #-}

module OpticsAndMonads where

import Control.Lens
  ( Ixed (ix),
    Magnify (magnify),
    Zoom (zoom),
    makeLenses,
    preview,
    use,
    uses,
    view,
    (+=),
    (.=),
    (<>=),
    (<~),
  )
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadIO (..), StateT, evalState, execState, execStateT, get, modify)
import Data.Map (Map)
import qualified Data.Map as M
import Text.Printf (printf)

--------------------------------------------------------------------------------------------
--                                13. Optics and Monads                                   --
--------------------------------------------------------------------------------------------

--------------------------------
-- 13.1 Reader Monad and View --
--------------------------------

type BenutzerName = String

type Password = String

data Env where
  Env ∷
    { _currentUser ∷ BenutzerName,
      _users ∷ Map BenutzerName Password
    } →
    Env
  deriving (Show)

makeLenses ''Env

-- using standard monad stack API
printUser ∷ ReaderT Env IO ()
printUser = do
  user ← asks _currentUser
  liftIO . putStrLn $ "Current user: " <> user

-- view is already in a monadic context
-- >>> :info view
-- view ∷ MonadReader s m ⇒ Getting a s a → m a

-- We can simply use `view`.
printUser' ∷ ReaderT Env IO ()
printUser' = do
  user ← view currentUser
  liftIO . putStrLn $ "Current user: " <> user

-- See `main` function for application!

-- `preview` works the same way!
getUserPassword ∷ ReaderT Env IO ()
getUserPassword = do
  userName ← view currentUser
  maybePassword ← preview (users . ix userName)
  liftIO $ print maybePassword

-- If you've got lenses defined for your environment using `view` is the idiomatic way of accessing your environment.

----------------------------------
-- 13.2 State Monad Combinators --
----------------------------------

data Till where
  Till ∷
    { _total ∷ Double,
      _sales ∷ [Double],
      _taxRate ∷ Double
    } →
    Till
  deriving (Show)

makeLenses ''Till

-- Almost ALL setter combinators have State equivalents which simply replace the `∼` with an `=`, e.g `.~` becomes `.=`!

saleCalculation ∷ StateT Till IO ()
saleCalculation = do
  total .= 0
  total += 8.55 ----------------------------------------- Delicious Hazy IPA
  total += 7.36 ----------------------------------------- Red Grapefruit Sour
  totalSale ← use total --------------------------------- `use` is like `view`, but for MonadState rather than MonadReader!
  liftIO $ printf "Total sale: $%.2f\n" totalSale
  sales <>= [totalSale]
  total <~ uses taxRate (totalSale *) ------------------- store (<~) it into our total using the total lens
  taxIncluded ← use total
  liftIO $ printf "Tax included: $%.2f\n" taxIncluded

-- e496
e496 ∷ IO Till
e496 = execStateT saleCalculation (Till 0 [] 1.19)

-- >>> till ← e496
-- Till {_total = 18.9329, _sales = [15.91], _taxRate = 1.19}

-- All of these MonadState combinators have alternate versions which return the existing or altered versions of the focus,
-- `(<+=)`, `(<<+=)`, `(<<∼)`, etc...

-- There are also combinators for dealing with MonadWriter but these come up rarely.

-----------------------------
-- 13.3 `magnify` & `zoom` --
-----------------------------

-- The lens library provides combinators which allow us to ‘re-scope’ a Reader (→ `magnify`) or State (→ `zoom`) monad to a portion of the type focused by a lens.

data Weather where
  Weather ∷
    { _temp ∷ Float,
      _pressure ∷ Float
    } →
    Weather
  deriving (Show)

makeLenses ''Weather

printData ∷ String → ReaderT Float IO ()
printData statName = do
  num ← ask ---------------------------------------- `num` can be any Float from any lens, i.e. `temp` or `pressure`.
  liftIO . putStrLn $ statName <> ": " <> show num

-- `magnify` is used for Reader monads.
--  magnify ∷ Lens' s a → ReaderT a m r → ReaderT s m r

-- embed printData
weatherStats ∷ ReaderT Weather IO ()
weatherStats = do
  magnify temp {-     -} (printData "temp") ------ `magnify` et. al. allow us to ‘re-scope’ a Reader or State monad to a portion of the type focused by a lens.
  magnify pressure {- -} (printData "pressure") -- `temp` and `pressure` can use the same function (`printData`).

-- By magnifying the Reader environment through a lens which focuses a Float we can run `printData` against that particular stat!
-- >>> runReaderT weatherStats (Weather 15 7.2)
-- temp: 15.0
-- pressure: 7.2

-- a State action which runs against our Weather object
convertCelsiusToFahrenheit ∷ StateT Float IO ()
convertCelsiusToFahrenheit = modify (\celsius → (celsius * (9 / 5)) + 32)

-- `zoom` is used for State monads.
-- zoom ∷ Monad m ⇒ Lens' s a → StateT a m r → StateT s m r

-- In order to run it in a State monad over the Weather type we'll need to zoom in on the temperature when we run it.
weatherStats' ∷ StateT Weather IO ()
weatherStats' = zoom temp convertCelsiusToFahrenheit

-- |
-- >>> execStateT weatherStats' (Weather 32 12)
-- Weather {_temp = 89.6, _pressure = 12.0}
