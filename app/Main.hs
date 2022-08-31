module Main where

import Control.Applicative (Applicative (liftA2), (<|>))
import Control.Arrow ((>>>))
import Control.Lens
import Control.Lens.Extras (biplate)
import Control.Lens.Unsound (lensProduct)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadIO (..), StateT, execStateT, modify)
import Data.Bits.Lens (bitAt)
import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Coerce (coerce)
import Data.Either.Validation (Validation (..))
import Data.Foldable (for_, toList)
import Data.List (intercalate, sort, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum))
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree (Node))
import Numeric.Lens (adding, dividing, multiplying, negated)
import Text.Printf (printf)

--------------------------------------------------------------------------------------------
--                                         Optics                                         --
--------------------------------------------------------------------------------------------

-- Nearly every data-related programming problem I’ve been faced with in the last year has had an elegant optics-based solution which typically takes less than a single line of code! (1)
-- `optics` are a family of inter-composable combinators for building bidirectional data transformations. (6)
-- They [optics] allow us to cleanly separate concerns in stronger ways than either of Object-Oriented or Functional-Programming styles allow on their own. (6)
-- Optics allow us to specify which portions of data we wish to work with SEPARATELY from the operations we wish to perform on them. (6)
-- Most tasks can be expressed in a single line of code, and in many cases the resulting code even reads like a simple sentence. (7)

----------------------------------
-- Practical Optics at a Glance --
----------------------------------

-- |
-- Update portions of immutable data structures.
-- >>> set _3 False ('a', 'b', 'c')
-- ('a','b',False)

-- |
-- We can perform a task over deeply nested subsets of data.
-- Let's sum all numbers in a `Left` within the right half of each tuple.
-- >>> sumOf (folded . _2 . _Left) [(True, Left 10), (False, Right "pepporoni"), (True, Left 20)]
-- 30

-- |
-- Truncate any stories longer than 10 characters, leaving shorter ones alone.
-- >>> stories = ["This one time at band camp", "Nuff!", "This is a short story"]
-- >>> over (traversed . filtered ((> 10) . length)) (\story → take 10 story ++ " ...") stories
-- ["This one t ...","Nuff!","This is a  ..."]

-- |
-- Truncate any stories longer than 10 characters, leaving shorter ones alone.
-- >>> stories = ["This one time at band camp", "Nuff!", "This is a short story"]
-- >>> over (traversed . filtered (length >>> (> 10))) (\story → take 10 story ++ " ...") stories
-- ["This one t ...","Nuff!","This is a  ..."]

------------------------------------
-- Impractical Optics at a Glance --
------------------------------------

-- |
-- Summarize a list of numbers, subtracting the 'Left's, adding the 'Right's!
-- >>> sumOf (folded . beside negated id) [Left 1, Right 10, Left 2, Right 20]
-- 27

-- |
-- Capitalize each word in a sentence.
-- >>> "why is a raven like a writing desk" & worded . _head %~ toUpper
-- "Why Is A Raven Like A Writing Desk"

-- |
-- Multiply every Integer by 100 no matter where they are in the structure.
-- >>> (Just 3, Left ("hello", [13, 15, 17])) & biplate *~ 100
-- (Just 300,Left ("hello",[1300,1500,1700]))

-- |
-- Reverse the ordering of all even numbers in a sequence.
-- We leave the odd numbers alone!
-- >>> [1, 2, 3, 4, 5, 6, 7, 8] & partsOf (traversed . filtered even) %~ reverse
-- [1,8,3,6,5,4,7,2]

-- |
-- Sort all the characters in all strings, across word boundaries!
-- >>> ("one", "two", "three") & partsOf (each . traversed) %~ sort
-- ("eee","hno","orttw")

-- |
-- Flip the 2nd bit of each number to a 0
-- >>> [1 ∷ Int, 2, 3, 4] & traversed . bitAt 1 %~ not
-- [3,0,1,6]

-- Prompt the user with each question in a tuple, then return the tuple with each prompt replaced with the user's input.
-- prompts = ("What is your name?" , "What is your quest?" , "What is your favourite color?")
-- prompts & each %%~ (\prompt → putStrLn prompt >> getLine)

-- What is your name?
-- > Sir Galahad
-- What is your quest?
-- > To seek the holy grail
-- What is your favourite color?
-- > Blue I think?
-- ("Sir Galahad","To seek the holy grail","Blue I think?")

--------------------------------------------------------------------------------------------
--                                         Lenses                                         --
--------------------------------------------------------------------------------------------

----------------------------
-- Introduction to Lenses --
----------------------------

-- Lenses have the following concrete guarantees:
-- • A Lens focuses (i.e. selects) a SINGLE piece of data within a larger structure.
-- • A Lens must NEVER FAIL to get or modify that focus.

-------------
-- Anatomy --
-------------

-- |
-- >>> view (_2 . _1) (42, ("hello", False))
-- "hello"

-- |
-- >>> view (_1 . _2) ((1, 2), 3)
-- 2

-- |
-- >>> set (_2 . _Left) "new" (False, Left "old")
-- (False,Left "new")

-- |
-- >>> over (taking 2 worded . traversed) toUpper "testing one two three"
-- "TESTING ONE two three"

-- |
-- >>> foldOf (both . each) (["super", "cali"],["fragilistic", "expialidocious"])
-- "supercalifragilisticexpialidocious"

-------------------------------
-- Exercises – Optic Anatomy --
-------------------------------

-- |
-- >>> view (_1 . _2) ((1, 2), 3)
-- 2

-- action: view
-- path: (_1 . _2)
-- structure: ((1, 2), 3)
-- focus: 2

-- |
-- >>> set (_2 . _Left) "new" (False, Left "old")
-- (False,Left "new")

-- action: set
-- path: (_2 . _Left)
-- structure: (False, Left "old")
-- focus: "old"

-- |
-- >>> over (taking 2 worded . traversed) toUpper "testing one two three"
-- "TESTING ONE two three"

-- action: over
-- path: (taking 2 worded . traversed)
-- structure: "testing one two three"
-- focus: "testing one"

-- |
-- >>> foldOf (both . each) (["super", "cali"],["fragilistic", "expialidocious"])
-- "supercalifragilisticexpialidocious"

-- action: foldOf
-- path: (both . each)
-- structure: (["super", "cali"],["fragilistic", "expialidocious"])
-- focus: "super", "cali", "fragilistic", "expialidocious"

------------------
-- Lens Actions --
------------------

----------------------------
-- Viewing through Lenses --
----------------------------

-- |
-- >>> view _1 ('a', 'b')
-- 'a'

-- |
-- >>> view _2 ('a', 'b')
-- 'b'

----------------------------
-- Setting through a Lens --
----------------------------

-- |
-- >>> set _1 'x' ('a', 'b')
-- ('x','b')

-- |
-- >>> over _1 (*100) (1, 2)
-- (100,2)

------------------------------
-- Exercises - Lens Actions --
------------------------------

-- 1. Find the structure and the focus in the following lens: Lens' (Bool, (Int, String)) Int
-- structure: (Bool, (Int, String))
-- focus: Int

-- 2. Write the type signature of a Lens with the structure (Char, Int) and the focus Char.
-- Lens' (Char, Int) Char

-- 3. Name 3 actions we can use on a Lens.
-- view, set, over

-- 4. Which lens could I use to focus the character ‘c’ in the following structure: ('a', 'b', 'c')
-- _3

-- 5. Write out the (simplified) types of each identifier in the following statement. What is the result of running it?
-- over ∷ Lens' (Bool, Int) Int → (Int → Int) → (Bool, Int) → (Bool, Int)

-- |
-- >>> over _2 (* 10) (False, 2)
-- (False,20)

------------------------
-- Lenses and Records --
------------------------

----------------------------------------
-- Building a Lens for a Record Field --
----------------------------------------

data Ship = Ship
  { _shipName ∷ String,
    _numCrew ∷ Int
  }
  deriving (Show)

purplePearl ∷ Ship
purplePearl =
  Ship
    { _shipName = "Purple Pearl",
      _numCrew = 38
    }

makeLenses ''Ship

-- `makeLenses` creates the following lenses using `lens`:
--
--                               Before structure type
--                                     |
--        Getter Fn      Setter Fn     | Before focus type
--            |              |         |   |
-- lens ∷ (s → a) → (s → b → t) → Lens s t a b
--                                       |   |
--                                       | After focus type
--                                       |
--                                 After structure type
--
-- or the simpler type signature:
--
--        Getter Fn   Setter Fn      Structure type
--            |          |              |
-- lens ∷ (s → a) → (s → a → s) → Lens' s a
--                                        |
--                                    Focus type
--
-- numCrew ∷ Lens' Ship Int
-- numCrew = lens _numCrew (\ship newNumCrew → ship {_numCrew = newNumCrew})

-- shipName ∷ Lens' Ship String
-- shipName = lens _shipName (\ship newShipName → ship {_shipName = newShipName})

----------------------------------
-- Exercises - Records Part One --
----------------------------------

-- 1. The structure and focus of a lens are typically represented by which letters in type signatures?
-- structure: s, t, ...
-- focus: a, b, ...

-- 2. Which two components are required to create a lens?
-- Setter and getter function.

-- 3. Implement the following lens: shipName' ∷ Lens' Ship String
-- See above for a more succinct solution.
shipName' ∷ Lens' Ship String
shipName' = lens getShipName setShipName
  where
    getShipName = _shipName
    setShipName ship newShipName = ship {_shipName = newShipName}

-------------------------------------------
-- Getting and Setting with a Field Lens --
-------------------------------------------

-- |
-- >>> purplePearl
-- Ship {_shipName = "Purple Pearl", _numCrew = 38}

-- |
-- >>> view numCrew purplePearl
-- 38

-- |
-- The same using the field accessor.
-- >>> _numCrew purplePearl
-- 38

-- |
-- >>> set numCrew 41 purplePearl
-- Ship {_shipName = "Purple Pearl", _numCrew = 41}

-- |
-- >>> set shipName "More Purple Pearl" purplePearl
-- Ship {_shipName = "More Purple Pearl", _numCrew = 38}

----------------------------------
-- Modifying Fields with a Lens --
----------------------------------

-- |
-- >>> over numCrew (+3) purplePearl
-- Ship {_shipName = "Purple Pearl", _numCrew = 41}

-- |
-- The same using `view` and `set` and running a function over the viewed value.
-- >>> set numCrew (view numCrew purplePearl + 3) purplePearl
-- Ship {_shipName = "Purple Pearl", _numCrew = 41}

-------------------------------------------
-- Automatically Generating Field Lenses --
-------------------------------------------

-- Automatically generated lenses by e.g. `makeLenses` can be seen in the repl with the browse command!

----------------------------------
-- Exercises - Records Part Two --
----------------------------------

-- 1. List the lenses which would be generated by the following `makeLenses` call, including their types.
--
-- data Inventory = Inventory
--   { _wand ∷ Wand,
--     _book ∷ Book,
--     _potions ∷ [Potion]
--   }

-- makeLenses ''Inventory

-- wand ∷ Lens' Inventory Wand
-- book ∷ Lens' Inventory Book
-- potions ∷ Lens' Inventory [Potion]

-- 2. Using the heuristics in this chapter, rewrite the following type as a Lens':
-- gazork ∷ Functor f ⇒ (Spuzz → f Spuzz) → Chumble → f Chumble
--
-- Fill in the blanks:
-- gazork ∷ Lens' _ _
-- gazork ∷ Lens' Chumble Spuzz

-- 3. The following code won’t compile! Can you see what’s wrong and fix the problem?
-- data Pet = Pet
--   { _petName ∷ String,
--     _petType ∷ String
--   }

-- getPetName ∷ Pet → String
-- getPetName pet = view petName pet

-- makeLenses ''Pet

-- The error says:
-- • Variable not in scope: petName ∷ Lens' Pet String

-- The `makeLenses` call should come right after the data definition of Pet because of TemplateHaskell splicing.

-----------------
-- Limitations --
-----------------

-------------------
-- Is it a Lens? --
-------------------

-- 1. Is it possible to build a lens which focuses the second element from a three-tuple?
-- second ∷ Lens' (a, b, c) b
-- Yes! Use _2.

-- 2. What about a lens which gets or sets the value inside a Maybe?
-- inMaybe ∷ Lens' (Maybe a) a
-- No! Maybe could be `Nothing`. No way to return an `a`.

-- 3. How about a lens which focuses on the value inside an Either?
-- left ∷ Lens' (Either a b) a
-- No! Basically the same as 2. If Either is a Right you're out of luck.

-- 4. How about this lens which focuses the third element of a list. Is listThird a valid lens?
-- listThird ∷ Lens' [a] a
-- No! List does not necessarily have a third element.

-- 5. Can you write a lens which focuses the second element of a tuple if the Bool in the first slot is True and focuses the third element if it's False?
-- conditional ∷ Lens' (Bool, a, a) a
-- Yes!

-- 6. Consider the following data type (which would generally be considered bad style):
-- data Err
--   = ReallyBadError {_msg ∷ String}
--   | ExitCode {_code ∷ Int}

-- makeLenses ''Err

-- Can you write a valid lens for?
-- msg ∷ Lens' Err String

-- No. Err could be ExitCode.

---------------
-- Lens Laws --
---------------

--------------------
-- Case Study: _1 --
--------------------

-- `_1` is a lawful lens.

-- |
-- 1st lens law:
-- You get back what you set (SET-GET)
-- >>> let newValue = "Firefly"
-- >>> view _1 (set _1 newValue ("Star Wars", "Star Trek"))
-- "Firefly"

-- |
-- >>> let newValue = "Firefly"
-- >>> view _1 (set _1 newValue ("Star Wars", "Star Trek")) == newValue
-- True

-- |
-- 2nd lens law:
-- Setting back what you got doesn't do anything (GET-SET)
-- No weird side-effects!
-- >>> let structure = ("Simpsons", "Seinfeld")
-- >>> set _1 (view _1 structure) structure
-- ("Simpsons","Seinfeld")

-- |
-- If we set the focus twice, only the last set should have any visible effect.
-- >>> let structure = ("Simpsons", "Seinfeld")
-- >>> set _1 (view _1 structure) structure == structure
-- True

-- |
-- 3rd lens law:
-- Setting twice is the same as setting once (SET-SET)
-- >>> let value1 = "Coffee"
-- >>> let value2 = "Red Bull"
-- >>> let structure = ("Beer", "Tea")
-- >>> set _1 value2 (set _1 value1 structure)
-- ("Red Bull","Tea")

-- |
-- >>> let value1 = "Coffee"
-- >>> let value2 = "Red Bull"
-- >>> let structure = ("Beer", "Tea")
-- >>> set _1 value2 structure
-- ("Red Bull","Tea")

-- |
-- >>> let value1 = "Coffee"
-- >>> let value2 = "Red Bull"
-- >>> let structure = ("Beer", "Tea")
-- >>> set _1 value2 (set _1 value1 structure) == set _1 value2 structure
-- True

-- You can assume most lenses included with an optics library will be lawful; if they aren't they'll likely
-- be in a separate `Unsafe` or `Unsound` module or will have documentation detailing how they break the laws.

---------------------
-- Case Study: msg --
---------------------

-- `msg` is unlawful.
-- Passes 2nd and 3rd law, but not the 1st.

---------
-- Err --
---------
data Err
  = ReallyBadError {_msg ∷ String}
  | ExitCode {_code ∷ Int}
  deriving (Eq)

-- makeLenses ''Err

msg ∷ Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg ∷ Err → String
    getMsg (ReallyBadError message) = message
    getMsg (ExitCode _) = "" -- Hrmm, I guess we just return ""?
    setMsg ∷ Err → String → Err
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    setMsg (ExitCode n) newMessage = ExitCode n -- Nowhere to set it, I guess we do nothing?

-- |
-- >>> let newMessage = "False alarm!"
-- >>> view msg (set msg newMessage (ReallyBadError "BAD BAD BAD"))
-- "False alarm!"

-- |
-- >>> let newMessage = "False alarm!"
-- >>> view msg (set msg newMessage (ReallyBadError "BAD BAD BAD")) == newMessage
-- True

-- |
-- Checking 1st lens law:
-- You get back what you set (set-get)
-- >>> let newMessage = "False alarm!"
-- >>> view msg (set msg newMessage (ExitCode 1))
-- ""

-- |
-- >>> let newMessage = "False alarm!"
-- >>> view msg (set msg newMessage (ExitCode 1)) == newMessage
-- False

-- |
-- Checking 2nd lens law:
-- Setting back what you got doesn't do anything (get-set)
-- >>> let err = ReallyBadError "BAD BAD BAD"
-- >>> set msg (view msg err) err == err
-- True

-- |
-- >>> let err = ExitCode 1
-- >>> set msg (view msg err) err == err
-- True

-- |
-- Checking 3rd lens law:
-- Setting twice is the same as setting once (set-set)
-- >>> let err = ReallyBadError "BAD BAD BAD"
-- >>> let value1 = "Value1"
-- >>> let value2 = "Value2"
-- >>> set msg value2 (set msg value1 err) == set msg value2 err
-- True

-- |
-- Checking 3rd lens law:
-- Setting twice is the same as setting once (set-set)
-- >>> let err = ExitCode 1
-- >>> let value1 = "Value1"
-- >>> let value2 = "Value2"
-- >>> set msg value2 (set msg value1 err) == set msg value2 err
-- True

-----------------------------
-- Case Study: lensProduct --
-----------------------------

-- A very useful law-breaking lens: lensProduct.
-- lensProduct ∷ Lens' s a → Lens' s b → Lens' s (a, b)

-- It allows you to take two lenses which accept the same structure to simultaneously focus two distinct
-- parts of it. That sounds reasonable enough, and indeed this situation comes up relatively often.

-------------
-- Session --
-------------
type UserName = String

type UserId = String

data Session = Session
  { _userId ∷ UserId,
    _userName ∷ UserName,
    _createdTime ∷ String,
    _expiryTime ∷ String
  }
  deriving (Show, Eq)

makeLenses ''Session

userInfo ∷ Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

-- |
-- `userId` and `userName` are disjoint:
-- >>> let session = Session "USER-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"
-- >>> view userInfo session
-- ("USER-1234","Joey Tribbiani")

-- Session and `userId` are not disjoint - they overlap!
alongsideUserId ∷ Lens' Session (Session, UserId)
alongsideUserId = lensProduct id userId

-- |
-- >>> let session = Session "USER-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"
-- >>> let newSession = session {_userId = "USER-5678"}
-- >>> view alongsideUserId (set alongsideUserId (newSession, "USER-9999") session)
-- (Session {_userId = "USER-9999", _userName = "Joey Tribbiani", _createdTime = "2019-07-25", _expiryTime = "2019-08-25"},"USER-9999")

-- |
-- 1st lens law
-- >>> let session = Session "USER-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"
-- >>> let newSession = session {_userId = "USER-5678"}
-- >>> view alongsideUserId (set alongsideUserId (newSession, "USER-9999") session) == (newSession, "USER-9999")
-- False

-- This time order has changed.
alongsideSession ∷ Lens' Session (UserId, Session)
alongsideSession = lensProduct userId id

-- |
-- value in the session overwrites the other
-- >>> let session = Session "USER-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"
-- >>> let newSession = session {_userId = "USER-5678"}
-- >>> view alongsideSession (set alongsideSession ("USER-9999", newSession) session)
-- ("USER-5678",Session {_userId = "USER-5678", _userName = "Joey Tribbiani", _createdTime = "2019-07-25", _expiryTime = "2019-08-25"})

-- |
-- >>> let session = Session "USER-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"
-- >>> let newSession = session {_userId = "USER-5678"}
-- >>> view alongsideSession (set alongsideSession ("USER-9999", newSession) session) == ("USER-9999", newSession)
-- False

----------------------
-- Exercises - Laws --
----------------------

-- 1. Implement a lens which breaks the second and/or third law. That's get-set and set-set respectively.

recorder ∷ Lens' ([a], a) a
recorder = lens getter setter
  where
    getter (_, a) = a
    setter (history, a) newVal = (a : history, newVal)

-- |
-- 1st law (set-get).
-- >>> let tstData = ([5, 4, 3, 2, 1], 5)
-- >>> let newValue = 6
-- >>> view recorder (set recorder newValue tstData) == newValue
-- True

-- |
-- 2nd law (get-set).
-- >>> let tstData = ([5, 4, 3, 2, 1], 5)
-- >>> let newValue = 6
-- >>> set recorder (view recorder tstData) tstData == tstData
-- False

-- |
-- 3rd law (set-set).
-- >>> let tstData = ([5, 4, 3, 2, 1], 5)
-- >>> let newValue = 6
-- >>> let anotherValue = 7
-- >>> set recorder newValue (set recorder anotherValue tstData) == set recorder newValue tstData
-- False

-- 2. Test the get-set and set-set laws for the `msg` lens we wrote this chapter. Does it pass these laws? See above - they pass.

--------------------
-- Virtual Fields --
--------------------

-----------------------------
-- Writing a Virtual Field --
-----------------------------

data Temperature = Temperature
  { _location ∷ String,
    _celsius ∷ Float
  }
  deriving (Show)

makeLenses ''Temperature

-- celsius ∷ Lens' Temperature Float

-- |
-- >>> let temp = Temperature "Berlin" 7.0
-- >>> view celsius temp
-- 7.0

-- |
-- >>> let temp = Temperature "Berlin" 7.0
-- >>> set celsius 13.5 temp
-- Temperature {_location = "Berlin", _celsius = 13.5}

-- |
-- Bump the temperature up by 10 degrees Celsius
-- >>> let temp = Temperature "Berlin" 7.0
-- >>> over celsius (+10) temp
-- Temperature {_location = "Berlin", _celsius = 17.0}

-- Conversion Functions
celsiusToFahrenheit ∷ Float → Float
celsiusToFahrenheit c = (c * (9 / 5)) + 32

fahrenheitToCelsius ∷ Float → Float
fahrenheitToCelsius f = (f - 32) * (5 / 9)

-- |
-- >>> let temp = Temperature "Berlin" 7.0
-- >>> view celsius temp & celsiusToFahrenheit
-- 44.6

-- |
-- >>> let temp = Temperature "Berlin" 7.0
-- >>> set celsius (fahrenheitToCelsius 56.3) temp
-- Temperature {_location = "Berlin", _celsius = 13.5}

-- |
-- Bump the temp by 18 degrees Fahrenheit
-- >>> let temp = Temperature "Berlin" 7.0
-- >>> over celsius (fahrenheitToCelsius . (+18) . celsiusToFahrenheit) temp
-- Temperature {_location = "Berlin", _celsius = 17.0}

-- virtual field (using existing lens)
-- `Temperature` doesn't have a field for Fahrenheit. We fake it by using the `celsius` lens to create a virtual field!
fahrenheit ∷ Lens' Temperature Float
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter temp f = set celsius (fahrenheitToCelsius f) temp

-- |
-- >>> let temp = Temperature "Berlin" 7.0
-- >>> view fahrenheit temp
-- 44.6

-- |
-- >>> let temp = Temperature "Berlin" 7.0
-- >>> set fahrenheit 56.3 temp
-- Temperature {_location = "Berlin", _celsius = 13.5}

-- |
-- >>> let temp = Temperature "Berlin" 7.0
-- >>> over fahrenheit (+ 18) temp
-- Temperature {_location = "Berlin", _celsius = 17.0}

--------------------------------
-- Exercises – Virtual Fields --
--------------------------------

data User = User
  { _firstName ∷ String,
    _lastName ∷ String,
    _userEmail ∷ String
  }
  deriving (Show)

makeLenses ''User

username ∷ Lens' User String
username = userEmail

fullName' ∷ Lens' User String
fullName' = lens getter setter
  where
    getter user = view firstName user <> (" " ∷ String) <> view lastName user
    setter user fname = set lastName (lstname fname) (set firstName (fstname fname) user)
      where
        fstname = head . words
        lstname = unwords . tail . words

-- |
-- >>> let user = User "John" "Cena" "invisible@example.com"
-- >>> view fullName' user
-- "John Cena"

-- |
-- >>> let user = User "John" "Cena" "invisible@example.com"
-- >>> set fullName' "Doctor of Thuganomics" user
-- User {_firstName = "Doctor", _lastName = "of Thuganomics", _userEmail = "invisible@example.com"}

------------------------------------------------
-- Data Correction and Maintaining Invariants --
------------------------------------------------

------------------------------------------
-- Including Correction Logic in Lenses --
------------------------------------------

data Time = Time
  { _hours ∷ Int,
    _mins ∷ Int
  }
  deriving (Show)

clamp ∷ Int → Int → Int → Int
clamp minVal maxVal = min maxVal . max minVal

hours ∷ Lens' Time Int
hours = lens getter setter
  where
    getter (Time h _) = h
    setter (Time _ m) newHours = Time (clamp 0 23 newHours) m

mins ∷ Lens' Time Int
mins = lens getter setter
  where
    getter (Time _ m) = m
    setter (Time h _) newMinutes = Time h (clamp 0 59 newMinutes)

-- |
-- >>> Time 3 10
-- Time {_hours = 3, _mins = 10}

-- |
-- >>> let time = Time 3 10
-- >>> set hours 40 time
-- Time {_hours = 23, _mins = 10}

-- |
-- >>> let time = Time 3 10
-- >>> set mins (-10) time
-- Time {_hours = 3, _mins = 0}

-- alternative implementations - unlawful lenses but helpful!
hours' ∷ Lens' Time Int
hours' = lens getter setter
  where
    getter (Time h _) = h
    setter (Time _ m) newHours = Time (newHours `mod` 24) m

mins' ∷ Lens' Time Int
mins' = lens getter setter
  where
    getter (Time _ m) = m
    setter (Time h _) newMinutes = Time ((h + (newMinutes `div` 60)) `mod` 24) (newMinutes `mod` 60)

-- |
-- >>> Time 3 10
-- Time {_hours = 3, _mins = 10}

-- |
-- >>> let time = Time 3 10
-- >>> over mins' (+ 55) time
-- Time {_hours = 4, _mins = 5}

-- |
-- >>> let time = Time 3 10
-- >>> over mins' (subtract 20) time
-- Time {_hours = 2, _mins = 50}

-- |
-- >>> over mins' (+1) (Time 23 59)
-- Time {_hours = 0, _mins = 0}

----------------------------------------
-- Exercises - Self-Correcting Lenses --
----------------------------------------

data ProducePrices = ProducePrices
  { _limePrice ∷ Float,
    _lemonPrice ∷ Float
  }
  deriving (Show)

limePrice ∷ Lens' ProducePrices Float
limePrice = lens getter setter
  where
    getter = _limePrice
    setter pp newPrice | newPrice < 0, _lemonPrice pp < 0.5 = pp {_limePrice = 0}
    setter pp newPrice | newPrice < 0 = pp {_limePrice = 0, _lemonPrice = 0.5}
    setter pp newPrice | abs (newPrice - _lemonPrice pp) < 0.5 = pp {_limePrice = newPrice}
    setter pp newPrice | newPrice - _lemonPrice pp > 0.5 = pp {_limePrice = newPrice, _lemonPrice = newPrice - 0.5}
    setter pp newPrice | _lemonPrice pp - newPrice > 0.5 = pp {_limePrice = newPrice, _lemonPrice = newPrice + 0.5}
    setter pp newPrice = pp {_limePrice = newPrice}

lemonPrice ∷ Lens' ProducePrices Float
lemonPrice = lens getter setter
  where
    getter = _lemonPrice
    setter pp newPrice | newPrice < 0, _limePrice pp < 0.5 = pp {_lemonPrice = 0}
    setter pp newPrice | newPrice < 0 = pp {_lemonPrice = 0, _limePrice = 0.5}
    setter pp newPrice | abs (newPrice - _limePrice pp) < 0.5 = pp {_lemonPrice = newPrice}
    setter pp newPrice | newPrice - _limePrice pp > 0.5 = pp {_lemonPrice = newPrice, _limePrice = newPrice - 0.5}
    setter pp newPrice | _limePrice pp - newPrice > 0.5 = pp {_lemonPrice = newPrice, _limePrice = newPrice + 0.5}
    setter pp newPrice = pp {_lemonPrice = newPrice}

-- |
-- >>> let prices = ProducePrices 1.50 1.48
-- >>> set limePrice 2 prices
-- ProducePrices {_limePrice = 2.0, _lemonPrice = 1.5}

-- |
-- >>> let prices = ProducePrices 1.50 1.48
-- >>> set limePrice 1.8 prices
-- ProducePrices {_limePrice = 1.8, _lemonPrice = 1.48}

-- |
-- >>> let prices = ProducePrices 1.50 1.48
-- >>> set limePrice 1.63 prices
-- ProducePrices {_limePrice = 1.63, _lemonPrice = 1.48}

-- |
-- >>> let prices = ProducePrices 1.50 1.48
-- >>> set limePrice (-1.00) prices
-- ProducePrices {_limePrice = 0.0, _lemonPrice = 0.5}

--------------------------------------------------------------------------------------------
--                                 Polymorphic Optics                                     --
--------------------------------------------------------------------------------------------

-----------------------------------------------------
-- Changing type variables with polymorphic lenses --
-----------------------------------------------------

data Promotion a = Promotion
  { _item ∷ a,
    _discountPercentage ∷ Double
  }
  deriving (Show)

item ∷ Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where
    getter ∷ Promotion a → a
    getter = _item

    setter ∷ Promotion a → b → Promotion b
    setter promo newItem = promo {_item = newItem}

-- |
-- >>> let peachPromo = Promotion "A really delicious Peach" 25.0
-- >>> peachPromo
-- Promotion {_item = "A really delicious Peach", _discountPercentage = 25.0}

-- |
-- let peachPromo = Promotion "A really delicious Peach" 25.0
-- :t peachPromo
-- peachPromo ∷ Promotion [Char]

-- |
-- let buffyFigurines = ["Buffy", "Angel", "Willow", "Giles"]
-- :t buffyFigurines
-- buffyFigurines ∷ [[Char]]

-- |
-- >>> let peachPromo = Promotion "A really delicious Peach" 25.0
-- >>> let buffyFigurines = ["Buffy", "Angel", "Willow", "Giles"]
-- >>> let buffyPromo = set item buffyFigurines peachPromo
-- >>> buffyPromo
-- Promotion {_item = ["Buffy","Angel","Willow","Giles"], _discountPercentage = 25.0}

-- |
-- let peachPromo = Promotion "A really delicious Peach" 25.0
-- let buffyFigurines = ["Buffy", "Angel", "Willow", "Giles"]
-- let buffyPromo = set item buffyFigurines peachPromo
-- :t buffyPromo
-- buffyPromo ∷ Promotion [[Char]]

-----------------
-- Preferences --
-----------------
data Preferences a = Preferences
  { _best ∷ a,
    _worst ∷ a
  }
  deriving (Show)

----------------------
-- Composing Lenses --
----------------------

------------------
-- Nested Types --
------------------
data Person = Person
  { _fullName ∷ String,
    _address ∷ Address
  }
  deriving (Show)

data Address = Address
  { _streetAddress ∷ StreetAddress,
    _city ∷ String,
    _country ∷ String
  }
  deriving (Show)

data StreetAddress = StreetAddress
  { _streetNumber ∷ String,
    _streetName ∷ String
  }
  deriving (Show)

makeLenses ''Person
makeLenses ''Address
makeLenses ''StreetAddress

sherlock ∷ Person
sherlock =
  Person
    { _fullName = "S. Holmes",
      _address =
        Address
          { _streetAddress =
              StreetAddress
                { _streetNumber = "221B",
                  _streetName = "Baker Street"
                },
            _city = "London",
            _country = "England"
          }
    }

-- using standard record update syntax to change street number
setStreetNumber ∷ String → Person → Person
setStreetNumber newStreetAddress person =
  let existingAddress = _address person
      existingStreetAddress = _streetAddress existingAddress
   in person
        { _address =
            existingAddress
              { _streetAddress =
                  existingStreetAddress
                    { _streetNumber = newStreetAddress
                    }
              }
        }

-- |
-- >>> setStreetNumber "221A" sherlock
-- Person {_fullName = "S. Holmes", _address = Address {_streetAddress = StreetAddress {_streetNumber = "221A", _streetName = "Baker Street"}, _city = "London", _country = "England"}}

--------------------------------
-- Composing Update Functions --
--------------------------------

-- Each of these functions accepts a function for modifying a single field and returns a modifying function over the larger record.

-- Update a Person's Address
updateAddress ∷ (Address → Address) → (Person → Person)
updateAddress modify existingPerson = existingPerson {_address = modify . _address $ existingPerson}

-- Update a Street Address within an Address
updateStreetAddress ∷ (StreetAddress → StreetAddress) → (Address → Address)
updateStreetAddress modify existingAddress = existingAddress {_streetAddress = modify . _streetAddress $ existingAddress}

-- Update a Street Number within a Streed Address
updateStreetNumber ∷ (String → String) → (StreetAddress → StreetAddress)
updateStreetNumber modify existingStreetAddress = existingStreetAddress {_streetNumber = modify . _streetNumber $ existingStreetAddress}

----------------------
-- Composing Lenses --
----------------------

-- Terminology:
-- modifier ∷ (a → a)
-- updater ∷ (a → a) → (s → s)
-- e.g. updateAddress = updater, (Address → Address) = modifier

-- |
-- Composing two updateres creates a new updater:
-- :t (updateStreetAddress . updateStreetNumber)
-- (updateStreetAddress . updateStreetNumber) ∷ (String → String) → Address → Address

-- |
-- :t (updateAddress . updateStreetAddress . updateStreetNumber)
-- (updateAddress . updateStreetAddress . updateStreetNumber) ∷ (String → String) → Person → Person

-- |
-- >>> (updateAddress . updateStreetAddress . updateStreetNumber) (const "221A") sherlock
-- Person {_fullName = "S. Holmes", _address = Address {_streetAddress = StreetAddress {_streetNumber = "221A", _streetName = "Baker Street"}, _city = "London", _country = "England"}}

-- |
-- address ∷ Lens' Person Address
-- :t address
-- address ∷ Functor f ⇒ (Address → f Address) → Person → f Person

-- |
-- streetAddress ∷ Lens' Address StreetAddress
-- :t streetAddress
-- streetAddress ∷ Functor f ⇒ (StreetAddress → f StreetAddress) → Address → f Address

-- |
-- streetNumber ∷ StreetAddress String
-- :t streetNumber
-- streetNumber ∷ Functor f ⇒ (String → f String) → StreetAddress → f StreetAddress

-- |
-- Lenses Compose:
-- address . streetAddress . streetNumber ∷ Lens' Person String
-- :t address . streetAddress . streetNumber
-- address . streetAddress . streetNumber ∷ Functor f ⇒ (String → f String) → Person → f Person

--------------------------------
-- How do Lens Types Compose? --
--------------------------------

----------
-- Game --
----------

-- Some dead-simple types which represent our game
data Player = Player deriving (Show)

data Wool = Wool deriving (Show)

data Sweater = Sweater deriving (Show)

data Item a = Item
  { _material ∷ a,
    _amount ∷ Int
  }
  deriving (Show)

makeLenses ''Item

-- This generates the following lenses:
-- material ∷ Lens (Item a) (Item b) a b
-- amount ∷ Lens' (Item a) Int

weave ∷ Wool → Sweater
weave Wool = Sweater

gameState ∷ (Player, Item Wool)
gameState = (Player, Item Wool 5)

-- |
-- >>> over (_2 . material) weave gameState
-- (Player,Item {_material = Sweater, _amount = 5})

--------------------------------------------------------------------------------------------
--                                       Operators                                        --
--------------------------------------------------------------------------------------------

--------------------
-- Lens Operators --
--------------------
data Payload = Payload {_weightKilos ∷ Int, _cargo ∷ String} deriving (Show)

newtype Boat = Boat {_payload ∷ Payload} deriving (Show)

makeLenses ''Payload
makeLenses ''Boat

serenity ∷ Boat
serenity = Boat (Payload 50000 "Livestock")

--------------------
-- view a.k.a. ^. --
--------------------

-- |
-- >>> view (payload . cargo) serenity
-- "Livestock"

-- |
-- >>> serenity ^. payload . cargo
-- "Livestock"

-- |
-- >>> serenity^.payload.cargo
-- "Livestock"

-------------------
-- set a.k.a. .∼ --
-------------------

-- |
-- >>> set (payload . cargo) "Medicine" serenity
-- Boat {_payload = Payload {_weightKilos = 50000, _cargo = "Medicine"}}

-- |
-- "Take serenity and then, regarding its payload's cargo, set it to Medicine"
-- >>> serenity & payload . cargo .~ "Medicine"
-- Boat {_payload = Payload {_weightKilos = 50000, _cargo = "Medicine"}}

-- |
-- Chain multiple set's:
-- >>> serenity & payload . cargo .~ "Chocolate" & payload . weightKilos .~ 2310
-- Boat {_payload = Payload {_weightKilos = 2310, _cargo = "Chocolate"}}

------------------------------
-- Chaining Many Operations --
------------------------------

-- |
-- Using traditional actions names:
-- >>> serenity & set (payload . cargo) "Chocolate" & set (payload . weightKilos) 2310
-- Boat {_payload = Payload {_weightKilos = 2310, _cargo = "Chocolate"}}

--------------------------
-- Using %∼ a.k.a. over --
--------------------------

-- |
-- %~ = over
-- >>> serenity & payload . weightKilos %~ subtract 1000 & payload . cargo .~ "Chocolate"
-- Boat {_payload = Payload {_weightKilos = 49000, _cargo = "Chocolate"}}

----------------------------
-- Learning Hieroglyphics --
----------------------------

-- |
-- >>> (2, 30) & _2 +~ 5
-- (2,35)

-- |
-- >>> (2, 30) & _2 -~ 5
-- (2,25)

-- |
-- >>> (2, 30) & _2 *~ 5
-- (2,150)

-- |
-- >>> (2, 30) & _2 //~ 5
-- (2,6.0)

-- |
-- >>> (2, 30) & _1 ^~ 3
-- (8,30)

-- |
-- >>> (False, 30) & _1 ||~ True
-- (True,30)

-- |
-- >>> (True, 30) & _1 &&~ True
-- (True,30)

-- |
-- >>> ("abra", 30) & _1 <>~ "cadabra"
-- ("abracadabra",30)

---------------
-- Modifiers --
---------------

-----------------
-- Thermometer --
-----------------
newtype Thermometer = Thermometer
  { _temperature ∷ Int
  }
  deriving (Show)

makeLenses ''Thermometer

-- |
-- >>> Thermometer 20 & temperature +~ 15
-- Thermometer {_temperature = 35}

-- |
-- Get new focus.
-- >>> Thermometer 20 & temperature <+~ 15
-- (35,Thermometer {_temperature = 35})

-- |
-- Get old focus.
-- >>> Thermometer 20 & temperature <<+~ 15
-- (20,Thermometer {_temperature = 35})

---------------------------------------------
-- When to use operators vs named actions? --
---------------------------------------------

-- Author finds it nicer to use the named versions when partially applying lens expressions, and use the operator versions the rest of the time.

-- |
-- >>> map (view _2) [("Patrick", "Star"), ("SpongeBob", "SquarePants")]
-- ["Star","SquarePants"]

-- |
-- Author thinks this looks a bit stupid.
-- >>> map (^. _2) [("Patrick", "Star"), ("SpongeBob", "SquarePants")]
-- ["Star","SquarePants"]

-- |
-- >>> map (over _2 reverse) [("Patrick", "Star"), ("SpongeBob", "SquarePants")]
-- [("Patrick","ratS"),("SpongeBob","stnaPerauqS")]

-- |
-- Author thinks this looks a bit stupid.
-- >>> map (_2 %~ reverse) [("Patrick", "Star"), ("SpongeBob", "SquarePants")]
-- [("Patrick","ratS"),("SpongeBob","stnaPerauqS")]

--------------------------------------------------------------------------------------------
--                                         Folds                                          --
--------------------------------------------------------------------------------------------

------------------------------------------
-- Focusing All Elements of a Container --
------------------------------------------

data Role
  = Gunner
  | PowderMonkey
  | Navigator
  | Captain
  | FirstMate
  deriving (Show, Eq, Ord)

data CrewMember = CrewMember
  { _crewMemberName ∷ String,
    _role ∷ Role,
    _talents ∷ [String]
  }
  deriving (Show, Eq, Ord)

makeLenses ''CrewMember

roster ∷ Set CrewMember
roster =
  S.fromList
    --              Name                    Role              Talents
    [ CrewMember "Grumpy Roger" {-    -} Gunner {-      -} ["Juggling", "Arbitrage"],
      CrewMember "Long-John Bronze" {--} PowderMonkey {--} ["Origami"],
      CrewMember "Salty Steve" {-     -} PowderMonkey {--} ["Charcuterie"],
      CrewMember "One-eyed Jack" {-   -} Navigator {-   -} []
    ]

------------------------
-- Collapsing the Set --
------------------------

crewMembers ∷ Fold (Set CrewMember) CrewMember
crewMembers = folded

-- |
-- >>> roster ^.. crewMembers
-- [CrewMember {_crewMemberName = "Grumpy Roger", _role = Gunner, _talents = ["Juggling","Arbitrage"]},CrewMember {_crewMemberName = "Long-John Bronze", _role = PowderMonkey, _talents = ["Origami"]},CrewMember {_crewMemberName = "One-eyed Jack", _role = Navigator, _talents = []},CrewMember {_crewMemberName = "Salty Steve", _role = PowderMonkey, _talents = ["Charcuterie"]}]

-- |
-- `Maybe` is Foldable!
-- >>> Just "Buried Treasure" ^.. folded
-- ["Buried Treasure"]

-- |
-- `folded` might focus zero elements
-- >>> Nothing ^.. folded
-- []

-- |
-- >>> Identity "Cutlass" ^.. folded
-- ["Cutlass"]

-- |
-- Remember that the Foldable instance of tuple only focuses the right-hand value
-- >>> ("Rubies", "Gold") ^.. folded
-- ["Gold"]

-- |
-- Folding a Map focuses only the values not the keys
-- >>> M.fromList [("Jack", "Captain"), ("Will", "First Mate")] ^.. folded
-- ["Captain","First Mate"]

-- Lenses can be used directly as folds!
crewRole ∷ Fold CrewMember Role
crewRole = role

-- |
-- >>> let jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]
-- >>> jerry ^. role
-- PowderMonkey

----------------------------------
-- Collecting Focuses as a List --
----------------------------------

-- Lenses must focus exactly one value, whereas folds can focus many.
-- Similarly, view (a.k.a. (^.)) always retrieves exactly one thing, but toListOf (a.k.a. (^..)) retrieves zero or more values in a list!

-- |
-- >>> let jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]
-- >>> jerry ^.. role
-- [PowderMonkey]

-- When we use a `lens` as a `fold` we can mentally substitute the types like this:
-- `Lens' s a` becomes `Fold s a`

---------------------
-- Composing Folds --
---------------------

-- |
-- >>> roster ^.. folded . role
-- [Gunner,PowderMonkey,Navigator,PowderMonkey]

-----------------------------------
-- Foundational Fold Combinators --
-----------------------------------

-- both & each

-- |
-- `both` on tuples focuses both at once
-- >>> ("Gemini", "Leo") ^.. both
-- ["Gemini","Leo"]

-- |
-- `both` on an Either type focuses whichever side is present
-- >>> Left "Albuquerque" ^.. both
-- ["Albuquerque"]

-- |
-- >>> Right "Yosemite" ^.. both
-- ["Yosemite"]

-- |
-- Only the last two type params of a tuple are 'bitraversable'
-- >>> ("Gemini", "Leo", "Libra") ^.. both
-- ["Leo","Libra"]

-- |
-- There's an `Each` instance for all reasonable sizes of tuples
-- >>> (1, 2, 3, 4, 5) ^.. each
-- [1,2,3,4,5]

-- |
-- Selects each element of a list
-- >>> [1, 2, 3, 4, 5] ^.. each
-- [1,2,3,4,5]

------------------
-- Custom Folds --
------------------

newtype Name = Name {getName ∷ String}
  deriving (Show)

data ShipCrew = ShipCrew
  { _ship ∷ Name,
    _captain ∷ Name,
    _firstMate ∷ Name,
    _conscripts ∷ [Name]
  }
  deriving (Show)

makeLenses ''ShipCrew

collectCrewMembers ∷ ShipCrew → [Name]
collectCrewMembers crew = [_captain crew, _firstMate crew] ++ _conscripts crew

allCrewMembers ∷ Fold ShipCrew Name
allCrewMembers = folding collectCrewMembers

myCrew ∷ ShipCrew
myCrew =
  ShipCrew
    { _ship = Name "Purple Pearl",
      _captain = Name "Grumpy Roger",
      _firstMate = Name "Long-John Bronze",
      _conscripts = [Name "One-eyed Jack", Name "Filthy Frank"]
    }

-- |
-- >>> myCrew ^.. allCrewMembers
-- [Name {getName = "Grumpy Roger"},Name {getName = "Long-John Bronze"},Name {getName = "One-eyed Jack"},Name {getName = "Filthy Frank"}]

-- =====================================================================================================================================

-- | RULE: Prefer many small and precise combinators which can be composed in different combinations to solve many different problems! |
-- =====================================================================================================================================

------------------------
-- Mapping over Folds --
------------------------

-- The to helper is pretty simple, it converts a function directly into a fold!

-- |
-- >>> Name "Two-faced Tony" ^. to getName
-- "Two-faced Tony"

-- |
-- We can chain many `to`s in a row
-- >>> Name "Two-faced Tony" ^. to getName . to (fmap toUpper)
-- "TWO-FACED TONY"

-- |
-- Or simply use function composition before passing to `to`
-- However, I find it confusing to switch from reading
-- left-to-right into right-to-left like this:
-- >>> Name "Two-faced Tony" ^. to (fmap toUpper . getName)
-- "TWO-FACED TONY"

-- |
-- `to` allows us to easily interleave function transformations into a path of composed optics.
-- >>> myCrew ^.. allCrewMembers . to getName
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]

----------------------------------------------------
-- Combining Multiple Folds on the Same Structure --
----------------------------------------------------

crewNames ∷ Fold ShipCrew Name
crewNames =
  folding
    ( \s →
        s ^.. captain
          <> s ^.. firstMate
          <> s ^.. conscripts . folded
    )

-- |
-- >>> myCrew ^.. crewNames . to getName
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]

--------------------------------
-- Writing Queries with Folds --
--------------------------------

-- |
-- Does my fold contain a given element?
-- >>> elemOf folded 3 [1, 2, 3, 4]
-- True

-- |
-- >>> elemOf folded 99 [1, 2, 3, 4]
-- False

-- |
-- Do ANY focuses match a predicate?
-- >>> anyOf folded even [1, 2, 3, 4]
-- True

-- |
-- >>> anyOf folded (>10) [1, 2, 3, 4]
-- False

-- |
-- Do ALL focuses match a predicate?
-- >>> allOf folded even [1, 2, 3, 4]
-- False

-- |
-- >>> allOf folded (<10) [1, 2, 3, 4]
-- True

-- |
-- Find the first element matching a predicate
-- >>> findOf folded even [1, 2, 3, 4]
-- Just 2

-- |
-- >>> findOf folded (>10) [1, 2, 3, 4]
-- Nothing

-- |
-- Does my fold have any elements?
-- >>> has folded []
-- False

-- |
-- >>> has folded [1, 2]
-- True

-- |
-- >>> hasn't folded []
-- True

-- |
-- >>> hasn't folded [1, 2]
-- False

-- |
-- How many focuses are there?
-- >>> lengthOf folded [1, 2, 3, 4]
-- 4

-- |
-- What's the sum of my focuses?
-- >>> sumOf folded [1, 2, 3, 4]
-- 10

-- |
-- What's the product of my focuses?
-- >>> productOf folded [1, 2, 3, 4]
-- 24

-- |
-- What's the first focus?
-- `firstOf`, `preview`, and `^?` are all effectively equivalent; use whichever you like.
-- >>> firstOf folded []
-- Nothing

-- |
-- >>> firstOf folded [1, 2, 3, 4]
-- Just 1

-- |
-- >>> preview folded [1, 2, 3, 4]
-- Just 1

-- |
-- >>> [1, 2, 3, 4] ^? folded
-- Just 1

-- |
-- What's the last focus?
-- >>> lastOf folded [1, 2, 3, 4]
-- Just 4

-- |
-- Find the minimum focus
-- >>> minimumOf folded [2, 1, 4, 3]
-- Just 1

-- |
-- >>> minimumOf folded []
-- Nothing

-- |
-- Find the maximum focus
-- >>> maximumOf folded [2, 1, 4, 3]
-- Just 4

-- |
-- >>> maximumOf folded []
-- Nothing

------------------------
-- Queries Case Study --
------------------------

-- TV Shows

data Actor = Actor
  { _actorName ∷ String,
    _birthYear ∷ Int
  }
  deriving (Show, Eq)

makeLenses ''Actor

data TVShow = TVShow
  { _title ∷ String,
    _numEpisodes ∷ Int,
    _numSeasons ∷ Int,
    _criticScore ∷ Double,
    _actors ∷ [Actor]
  }
  deriving (Show, Eq)

makeLenses ''TVShow

howIMetYourMother ∷ TVShow
howIMetYourMother =
  TVShow
    { _title = "How I Met Your Mother",
      _numEpisodes = 208,
      _numSeasons = 9,
      _criticScore = 83,
      _actors =
        [ Actor "Josh Radnor" 1974,
          Actor "Cobie Smulders" 1982,
          Actor "Neil Patrick Harris" 1973,
          Actor "Alyson Hannigan" 1974,
          Actor "Jason Segel" 1980
        ]
    }

buffy ∷ TVShow
buffy =
  TVShow
    { _title = "Buffy the Vampire Slayer",
      _numEpisodes = 144,
      _numSeasons = 7,
      _criticScore = 81,
      _actors =
        [ Actor "Sarah Michelle Gellar" 1977,
          Actor "Alyson Hannigan" 1974,
          Actor "Nicholas Brendon" 1971,
          Actor "David Boreanaz" 1969,
          Actor "Anthony Head" 1954
        ]
    }

tvShows ∷ [TVShow]
tvShows = [howIMetYourMother, buffy]

-- |
-- >>> sumOf (folded . numEpisodes) tvShows
-- 352

-- |
-- >>> sum $ tvShows ^.. folded . numEpisodes
-- 352

-- |
-- >>> maximumOf (folded . criticScore) tvShows
-- Just 83.0

-- |
-- >>> _title <$> maximumByOf folded (comparing _criticScore) tvShows
-- Just "How I Met Your Mother"

-- |
-- >>> minimumByOf (folded . actors . folded) (comparing _birthYear) tvShows
-- Just (Actor {_actorName = "Anthony Head", _birthYear = 1954})

-- `comparingOf` accepts a lens and will return the sort of comparator function that actions like `minimumByOf` expects.
comparingOf ∷ Ord a ⇒ Lens' s a → (s → s → Ordering)
comparingOf l = comparing (view l)

-- |
-- >>> minimumByOf (folded . actors . folded) (comparingOf birthYear) tvShows
-- Just (Actor {_actorName = "Anthony Head", _birthYear = 1954})

--------------------------
-- Folding with Effects --
--------------------------

calcAge ∷ Actor → Int
calcAge actor = 2030 - _birthYear actor

showActor ∷ Actor → String
showActor actor = _actorName actor <> ": " <> show (calcAge actor)

-- |
-- >>> traverseOf_ (folded . actors . folded . to showActor) putStrLn tvShows
-- Josh Radnor: 56
-- Cobie Smulders: 48
-- Neil Patrick Harris: 57
-- Alyson Hannigan: 56
-- Jason Segel: 50
-- Sarah Michelle Gellar: 53
-- Alyson Hannigan: 56
-- Nicholas Brendon: 59
-- David Boreanaz: 61
-- Anthony Head: 76

-- |
-- Oftentimes it's handy to build up a stateful computation from the focuses of a fold.
-- >>> import Control.Monad.State
-- >>> execState (traverseOf_ folded (modify . const (+ 1)) tvShows) 0
-- 2

----------------------------
-- Combining Fold Results --
----------------------------

-- |
-- >>> (Sum 1, Sum 32) <> (Sum 1, Sum 20)
-- (Sum {getSum = 2},Sum {getSum = 52})

-- transform actor to monoid
ageSummary ∷ Actor → (Sum Int, Sum Int)
ageSummary actor = (Sum 1, Sum (calcAge actor))

-- |
-- >>> foldOf (folded . actors . folded . to ageSummary) tvShows
-- (Sum {getSum = 10},Sum {getSum = 572})

-- compute average age
computeAverage ∷ (Sum Int, Sum Int) → Double
computeAverage (Sum count, Sum total) = fromIntegral total / fromIntegral count

-- |
-- >>> computeAverage $ foldOf (folded . actors . folded . to ageSummary) tvShows
-- 57.2

-- |
-- >>> computeAverage $ foldMapOf (folded . actors . folded) ageSummary tvShows
-- 57.2

---------------------------
-- Using ‘view’ on Folds --
---------------------------

-- A very common mistake when people get started with folds is to accidentally use (^.) or view on a
-- fold instead of a lens. This is especially confusing because it actually works in some cases but not
-- others.

-- |
-- This works just fine
-- >>> Just "do it" ^. folded
-- "do it"

-- This one crashes and burns!
-- >>> Just (42 ∷ Int) ^. folded
-- No instance for (Monoid Int) arising from a use of ‘folded’

-- |
-- When there's a single focus, we just return it
-- >>> Just "do it" ^. folded
-- "do it"

-- |
-- When there aren't any focuses, return 'mempty'
-- >>> Nothing ^. folded ∷ String
-- ""

-- |
-- When there are multiple focuses, combine them with (<>).
-- >>> ("one", "two", "three") ^. each
-- "onetwothree"

-- In general you should avoid this "weird" behaviour and just use `foldOf` explicitly when you want this behaviour.

--------------------------------
-- Customizing Monoidal Folds --
--------------------------------

-- |
-- >>> foldMapOf (folded . actors . folded . actorName) (\n → M.singleton n 1) tvShows
-- fromList [("Alyson Hannigan",1),("Anthony Head",1),("Cobie Smulders",1),("David Boreanaz",1),("Jason Segel",1),("Josh Radnor",1),("Neil Patrick Harris",1),("Nicholas Brendon",1),("Sarah Michelle Gellar",1)]

-- |
-- When we combine two maps with the same keys it simply ignores duplicate values.
-- >>> M.singleton 'a' "first" <> M.singleton 'a' "second"
-- fromList [('a',"first")]

-- |
-- This looks better:
-- >>> M.unionWith (+) (M.singleton "an actor" 1) (M.singleton "an actor" 1)
-- fromList [("an actor",2)]

-- |
-- "Alyson Hannigan" is in both shows.
-- >>> foldMapByOf (folded . actors . folded . actorName) (M.unionWith (+)) mempty (\n → M.singleton n 1) tvShows
-- fromList [("Alyson Hannigan",2),("Anthony Head",1),("Cobie Smulders",1),("David Boreanaz",1),("Jason Segel",1),("Josh Radnor",1),("Neil Patrick Harris",1),("Nicholas Brendon",1),("Sarah Michelle Gellar",1)]

------------------------
-- Higher Order Folds --
------------------------

----------------------
-- Taking, Dropping --
----------------------

-- |
-- >>> [1, 2, 3, 4] ^.. taking 2 folded
-- [1,2]

-- |
-- >>> [1, 2, 3, 4] ^.. dropping 2 folded
-- [3,4]

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded . taking 2 folded
-- [1,2,10,20,100,200]

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. taking 2 folded
-- [[1,2,3],[10,20,30]]

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded
-- [[1,2,3],[10,20,30],[100,200,300]]

-- |
-- >>> ("Albus", "Dumbledore") ^.. both . taking 3 folded
-- "AlbDum"

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded
-- [[1,2,3],[10,20,30],[100,200,300]]

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. (folded . folded)
-- [1,2,3,10,20,30,100,200,300]

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. taking 2 (folded . folded)
-- [1,2]

-- |
-- >>> ("Albus", "Dumbledore") ^.. folded
-- ["Dumbledore"]

-- |
-- >>> ("Albus", "Dumbledore") ^.. both . folded
-- "AlbusDumbledore"

-- |
-- >>> ("Albus", "Dumbledore") ^.. taking 3 (both . folded)
-- "Alb"

-- |
-- >>> ("Albus", "Dumbledore") ^.. taking 3 both . folded
-- "AlbusDumbledore"

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. (taking 2 folded)
-- [[1,2,3],[10,20,30]]

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. (taking 2 folded) . folded
-- [1,2,3,10,20,30]

-- |
-- >>> (["Albus", "Dumbledore"], ["Severus", "Snape"]) ^.. taking 3 (both . folded)
-- ["Albus","Dumbledore","Severus"]

-- |
-- >>> (["Albus", "Dumbledore"], ["Severus", "Snape"]) ^.. taking 3 (both . folded) . folded
-- "AlbusDumbledoreSeverus"

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. dropping 2 (folded . folded)
-- [3,10,20,30,100,200,300]

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded . dropping 2 folded
-- [3,30,300]

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. dropping 2 folded . folded
-- [100,200,300]

-- |
-- >>> ("Albus", "Dumbledore") ^.. both . dropping 2 folded
-- "busmbledore"

-- |
-- >>> ("Albus", "Dumbledore") ^.. both . folded
-- "AlbusDumbledore"

---------------
-- Backwards --
---------------

-- |
-- >>> [1, 2, 3] ^.. backwards folded
-- [3,2,1]

-- |
-- >>> ("one", "two") ^.. backwards both
-- ["two","one"]

-- |
-- >>> [(1, 2), (3, 4)] ^.. folded . both
-- [1,2,3,4]

-- |
-- >>> [(1, 2), (3, 4)] ^.. backwards (folded . both)
-- [4,3,2,1]

-- |
-- >>> [(1, 2), (3, 4)] ^.. backwards folded . both
-- [3,4,1,2]

-- |
-- >>> [(1, 2), (3, 4)] ^.. folded . backwards both
-- [2,1,4,3]

--------------------------------
-- TakingWhile, DroppingWhile --
--------------------------------

-- |
-- >>> [1, 5, 15, 5, 1] ^.. takingWhile (<10) folded
-- [1,5]

-- |
-- >>> [1..100] ^.. takingWhile (<10) folded
-- [1,2,3,4,5,6,7,8,9]

-- |
-- >>> [1..] ^.. takingWhile (<10) folded
-- [1,2,3,4,5,6,7,8,9]

-- |
-- >>> [1..100] ^.. droppingWhile (<90) folded
-- [90,91,92,93,94,95,96,97,98,99,100]

-- |
-- >>> [1, 5, 15, 5, 1] ^.. droppingWhile (<10) folded
-- [15,5,1]

---------------------
-- Filtering Folds --
---------------------

-- |
-- >>> [1, 2, 3, 4] ^.. folded . filtered even
-- [2,4]

-- |
-- >>> ["apple", "passionfruit", "orange", "pomegranate"] ^.. folded . filtered ((> 6) . length)
-- ["passionfruit","pomegranate"]

-----------
-- Cards --
-----------

-- A data structure to represent a single card
data Card = Card
  { _cardName ∷ String,
    _aura ∷ Aura,
    _holo ∷ Bool,
    _moves ∷ [Move]
  }
  deriving (Show, Eq)

-- Each card has an aura-type
data Aura
  = Wet
  | Hot
  | Spark
  | Leafy
  deriving (Show, Eq)

-- Cards have attack moves
data Move = Move
  { _moveName ∷ String,
    _movePower ∷ Int
  }
  deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

deck ∷ [Card]
deck =
  [ Card "Skwortul" {-   -} Wet {-    -} False {--} [Move "Squirt" 20],
    Card "Scorchander" {--} Hot {-    -} False {--} [Move "Scorch" 20],
    Card "Seedasaur" {-  -} Leafy {-  -} False {--} [Move "Allergize" 20],
    Card "Kapichu" {-    -} Spark {-  -} False {--} [Move "Poke" 10, Move "Zap" 30],
    Card "Elecdude" {-   -} Spark {-  -} False {--} [Move "Asplode" 50],
    Card "Garydose" {-   -} Wet {-    -} True {- -} [Move "Gary's move" 40],
    Card "Moisteon" {-   -} Wet {-    -} False {--} [Move "Soggy" 3],
    Card "Grasseon" {-   -} Leafy {-  -} False {--} [Move "Leaf Cut" 30],
    Card "Spicyeon" {-   -} Hot {-    -} False {--} [Move "Capsaicisize" 40],
    Card "Sparkeon" {-   -} Spark {-  -} True {- -} [Move "Shock" 40, Move "Battery" 50]
  ]

-- |
-- How many Spark Cards do I have?
-- >>> lengthOf (folded . aura . filtered (== Spark)) deck
-- 3

-- |
-- How many moves have an attack power above 30?
-- >>> lengthOf (folded . moves . folded . movePower . filtered (> 30)) deck
-- 5

-- |
-- >>> lengthOf (folded . moves ) deck
-- 10

-- |
-- >>> deck ^.. folded . moves . folded . movePower
-- [20,20,20,10,30,50,40,3,30,40,40,50]

-- |

--- List all cards which have ANY move with an attack power greater than 40
-- >>> deck ^.. folded . filtered (anyOf (moves . folded . movePower) (> 40)) . cardName
-- ["Elecdude","Sparkeon"]

-- |
-- How many moves do my Spark cards have in total?
-- >>> lengthOf ( folded . filtered ((== Spark) . _aura) . moves . folded ) deck
-- 5

-- |
-- List all my Spark Moves with a power greater than 30
-- >>> deck ^.. folded . filtered ((== Spark) . _aura) . moves . folded . filtered ((> 30) . _movePower) . moveName
-- ["Asplode","Shock","Battery"]

-- |
-- Using `filteredBy` we can pass a fold instead of a predicate!
-- We can continue to think in folds and keep reading left-to-right.
-- >>> deck ^.. folded . filteredBy (aura . only Spark) . moves . folded . filteredBy (movePower . filtered (>30)) . moveName
-- ["Asplode","Shock","Battery"]

-- `only` is a utility fold.
-- It accepts a reference value and will return a fold which yields a `()` if and only if the input value is equal to the reference value.

-- |
-- >>> 1 ^? only 1
-- Just ()

-- |
-- >>> 2 ^? only 1
-- Nothing

-- |
-- >>> has (only "needle") "needle"
-- True

-- |
-- >>> has (only "needle") "haystack"
-- False

-- |
-- Get the holographic card which has the largest number of moves
-- >>> maximumByOf (folded . filtered _holo) (comparing (lengthOf moves)) deck
-- Just (Card {_cardName = "Sparkeon", _aura = Spark, _holo = True, _moves = [Move {_moveName = "Shock", _movePower = 40},Move {_moveName = "Battery", _movePower = 50}]})

--------------------------------------------------------------------------------------------
--                                       Traversals                                       --
--------------------------------------------------------------------------------------------

-----------------------------------------------
-- How do Traversals Fit into the Hierarchy? --
-----------------------------------------------

--            | Get    | Set/Modify | Traverse
-----------------------------------------------
-- Lens       | Single |   Single   | Single
-- Fold       |  Many  |     ✗      |   ✗
-- Traversal  |  Many  |   Many     | Many

-- A Traversal can be used as a Fold but not vice versa.
-- A Fold can only be used as a Fold.
-- A Lens can be used as anything.
--
-- can be used as | Lens | Fold | Traversal
-- -------------- | -----| -----| ----------
-- Lens           |  ✓   |  ✓   |    ✓
-- Fold           |  ✗   |  ✓   |    ✗
-- Traversal      |  ✗   |  ✓   |    ✓

----------------------------
-- From Fold to Traversal --
----------------------------

-- |
-- we can use the traversal as a fold
-- >>> ("Bubbles", "Buttercup") ^.. both
-- ["Bubbles","Buttercup"]

-- |
-- modify the focuses using the over action: (%∼)
-- >>> ("Bubbles", "Buttercup") & both %~ (++ "!")
-- ("Bubbles!","Buttercup!")

-- |
-- using set (.∼) on a traversal
-- >>> ("Bubbles", "Buttercup") & both .~ "Blossom"
-- ("Blossom","Blossom")

-- |
-- changing type of tuple (polymorphic traversal)
-- >>> ("Bubbles", "Buttercup") & both %~ length
-- (7,9)

-- |
-- each is also a traversal
-- >>> (1, 2, 3) & each %~ (*10)
-- (10,20,30)

-- |
-- ("Here's Johnny" ∷ Text) & each %~ toUpper
-- "HERE'S JOHNNY"

-- |
-- you can't change what Text is made of
-- ("Houston we have a problem" ∷ Text) & each .~ (22 ∷ Int)
-- Couldn't match expected type ‘Text’ with actual type ‘[Char]’
-- Couldn't match type ‘Int’ with ‘Char’ arising from a use of ‘each’

-- |
-- >>> [1, 2, 3, 4, 5] & taking 3 traversed *~ 10
-- [10,20,30,4,5]

-- |
-- >>> [1, 2, 3, 4, 5] & dropping 3 traversed *~ 10
-- [1,2,3,40,50]

-- |
-- >>> "once upon a time - optics became mainstream" & takingWhile (/= '-') traversed %~ toUpper
-- "ONCE UPON A TIME - optics became mainstream"

-- |
-- Multiply all even numbers by 10
-- >>> [1, 2, 3, 4, 5] & traversed . filtered even *~ 10
-- [1,20,3,40,5]

-- |
-- `filtered` is an extremely powerful tool, it alters the exact same elements which would be focused when you use it in a fold.
-- Reverse only the long strings
-- >>> ("short", "really long") & both . filtered ((> 5) . length) %~ reverse
-- ("short","gnol yllaer")

---------------------------
-- Traversal Combinators --
---------------------------

--------------------------------------------
-- Traversing Each Element of a Container --
--------------------------------------------

-- |
-- cannot modify a fold
-- [1, 2, 3] & folded %~ (*10)
-- Could not deduce (Contravariant Identity)
--   arising from a use of ‘folded’

-- `folded` can be used on more container types (like `Set`), but `traversed` has strictly more power (it can set and update).

-- |
-- >>> [1, 2, 3] & traversed *~ 10
-- [10,20,30]

-- |
-- Tuples are traversable over their last slot
-- >>> ("Batman", "Superman") & traversed %~ take 3
-- ("Batman","Sup")

-- |
-- >>> let powerLevels = M.fromList [("Gohan", 710) , ("Goku", 9001) , ("Krillin", 5000) , ("Piccolo", 408)]
-- >>> powerLevels & traversed %~ \n → if n > 9000 then "Over 9000" else show n
-- fromList [("Gohan","710"),("Goku","Over 9000"),("Krillin","5000"),("Piccolo","408")]

-- |
-- >>> let opticsTree = Node "Lens" [Node "Fold" [], Node "Traversal" []]
-- >>> opticsTree & traversed %~ reverse
-- Node {rootLabel = "sneL", subForest = [Node {rootLabel = "dloF", subForest = []},Node {rootLabel = "lasrevarT", subForest = []}]}

----------------------
-- More Combinators --
----------------------

-- |
-- as folds `worded` works pretty much the same as `words`
-- >>> "I'll be back!" ^.. worded
-- ["I'll","be","back!"]

-- |
-- the same for lined
-- >>> "Run\nForrest\nRun" ^.. lined
-- ["Run","Forrest","Run"]

-- |
-- as traversal we can also update
-- Surround each word with '*'s
-- result is a String
-- >>> "blue suede shoes" & worded %~ \s → "*" ++ s ++ "*"
-- "*blue* *suede* *shoes*"

-- |
-- Capitalize each word
-- >>> "blue suede shoes" & worded %~ \(x:xs) → toUpper x : xs
-- "Blue Suede Shoes"

-- |
-- Add a "#" to the start of each line:
-- >>> "blue\nsuede\nshoes" & lined %~ ('#':)
-- "#blue\n#suede\n#shoes"

-- |
-- Mapping the identity function still has the white-space collapsing side-effects of `unwords`.
-- newlines are getting lost
-- >>> "blue \n suede \n \n shoes" & worded %~ id
-- "blue suede shoes"

---------------------------------------
-- Traversing Multiple Paths at Once --
---------------------------------------

-- |
-- >>> let dinos = ("T-Rex", (42, "Stegosaurus"))
-- >>> dinos ^.. beside id _2
-- ["T-Rex","Stegosaurus"]

-- |
-- We can provide a different path to focus Ints for each half of the tuple.
-- >>> let numbers = ([(1, 2), (3, 4)], [5, 6, 7])
-- >>> numbers ^.. beside (traversed . both) traversed
-- [1,2,3,4,5,6,7]

-- |
-- >>> ("T-Rex", ("Ankylosaurus", "Stegosaurus")) ^.. beside id both
-- ["T-Rex","Ankylosaurus","Stegosaurus"]

-- |
-- We can modify all characters inside both halves of the tuple
-- Each half of the tuple has a different path to focus the characters
-- >>> ("Cowabunga", ["let's", "order", "pizza"]) & beside traversed (traversed . traversed) %~ toUpper
-- ("COWABUNGA",["LET'S","ORDER","PIZZA"])

-- |
-- Every `Bitraversable` can be used with `beside`.
-- >>> Left (1, 2) & beside both traversed %~ negate
-- Left (-1,-2)

-- |
-- >>> Right [3, 4] & beside both traversed %~ negate ∷ Either (Int, Int) [Int]
-- Right [-3,-4]

-------------------------------------------
-- Focusing a Specific Traversal Element --
-------------------------------------------

-- |
-- >>> [0, 1, 2, 3, 4] ^? element 2
-- Just 2

-- |
-- `element` can't change the container's element type.
-- This is called a `monomorphic traversal`.
-- >>> [0, 1, 2, 3, 4] & element 2 *~ 100
-- [0,1,200,3,4]

-- |
-- `element` is basically `elementOf traversed`
-- >>> [0, 1, 2, 3, 4] ^? elementOf traversed 2
-- Just 2

-- |
-- We can get a specific element from a composition of traversals
-- >>> [[0, 1, 2], [3, 4], [5, 6, 7, 8]] ^? elementOf (traversed . traversed) 6
-- Just 6

-- |
-- >>> [[0, 1, 2], [3, 4], [5, 6, 7, 8]] & elementOf (traversed . traversed) 6 *~ 100
-- [[0,1,2],[3,4],[5,600,7,8]]

---------------------------
-- Traversal Composition --
---------------------------

-- |
-- Capitalize the first char of every word.
-- >>> "blue suede shoes" & worded . taking 1 traversed %~ toUpper
-- "Blue Suede Shoes"

-- |
-- Find all strings longer than 5 chars then surround each word in that string with '*'
-- >>> ["short", "really long"] & traversed . filtered ((> 5) . length) . worded %~ \s → "*" ++ s ++ "*"
-- ["short","*really* *long*"]

-- |
-- Add "Rich " to the names of people with more than $1000.
-- >>> (("Ritchie", 100000), ("Archie", 32), ("Reggie", 4350)) & each . filtered ((> 1000) . snd) . _1 %~ ("Rich " ++)
-- (("Rich Ritchie",100000),("Archie",32),("Rich Reggie",4350))

-----------------------
-- Traversal Actions --
-----------------------

-----------------------------
-- A Primer on Traversable --
-----------------------------

-- |
-- >>> sequenceA [Just 1, Just 2, Just 3]
-- Just [1,2,3]

-- |
-- >>> sequenceA [Just 1, Nothing, Just 3]
-- Nothing

-- |
-- >>> sequenceA $ Just (Left "Whoops")
-- Left "Whoops"

-- :t readMaybe
-- readMaybe ∷ Read a ⇒ String → Maybe a
-- 'readMaybe' is polymorphic so we need to specify a concrete result type

-- |
-- >>> import Text.Read (readMaybe)
-- >>> traverse readMaybe ["1", "2", "3"] ∷ Maybe [Int]
-- Just [1,2,3]

-- |
-- >>> import Text.Read (readMaybe)
-- >>> traverse readMaybe ["1", "snark", "3"] ∷ Maybe [Int]
-- Nothing

-- |
-- >>> traverse (\n → [n * 10, n * 100]) ("a", 10)
-- [("a",100),("a",1000)]

----------------------------
-- Traverse on Traversals --
----------------------------

-- |
-- >>> import Text.Read (readMaybe)
-- >>> traverseOf both readMaybe ("1", "2") ∷ Maybe (Int, Int)
-- Just (1,2)

-- |
-- >>> import Text.Read (readMaybe)
-- >>> traverseOf both readMaybe ("not a number", "2") ∷ Maybe (Int, Int)
-- Nothing

-- |
-- >>> traverseOf both (\c → [toLower c, toUpper c]) ('a', 'b')
-- [('a','b'),('a','B'),('A','b'),('A','B')]

-- |
-- >>> traverseOf (both . traversed) (\c → [toLower c, toUpper c]) ("ab", "cd")
-- [("ab","cd"),("ab","cD"),("ab","Cd"),("ab","CD"),("aB","cd"),("aB","cD"),("aB","Cd"),("aB","CD"),("Ab","cd"),("Ab","cD"),("Ab","Cd"),("Ab","CD"),("AB","cd"),("AB","cD"),("AB","Cd"),("AB","CD")]

-- simple validation
validateEmail ∷ String → Either String String
validateEmail email
  | '@' `elem` email = Right email
  | otherwise = Left ("missing '@': " <> email)

-- |
-- >>> traverseOf (traversed . _2) validateEmail [ ("Mike", "mike@tmnt.io") , ("Raph", "raph@tmnt.io") , ("Don", "don@tmnt.io") , ("Leo", "leo@tmnt.io") ]
-- Right [("Mike","mike@tmnt.io"),("Raph","raph@tmnt.io"),("Don","don@tmnt.io"),("Leo","leo@tmnt.io")]

-- |
-- >>> traverseOf (traversed . _2) validateEmail [ ("Mike", "mike@tmnt.io") , ("Raph", "raph.io") , ("Don", "don@tmnt.io") , ("Leo", "leo@tmnt.io") ]
-- Left "missing '@': raph.io"

-- simple validation
validateEmail' ∷ String → Validation [String] String
validateEmail' email
  | '@' `elem` email = Success email
  | otherwise = Failure ["missing '@': " <> email]

-- |
-- >>> traverseOf (both . traversed) validateEmail' (["mike@tmnt.io", "raph@tmnt.io"], ["don@tmnt.io", "leo@tmnt.io"])
-- Success (["mike@tmnt.io","raph@tmnt.io"],["don@tmnt.io","leo@tmnt.io"])

-- |
-- >>> traverseOf (both . traversed) validateEmail' (["mike@tmnt.io", "raph.io"], ["don@tmnt.io", "leo.io"])
-- Failure ["missing '@': raph.io","missing '@': leo.io"]

-- |
-- >>> sequenceAOf _1 (Just "Garfield", "Lasagna")
-- Just ("Garfield","Lasagna")

-- |
-- >>> sequenceAOf _1 (Nothing, "Lasagna")
-- Nothing

-- |
-- >>> sequenceAOf (both . traversed) ([Just "apples"], [Just "oranges"])
-- Just (["apples"],["oranges"])

-- |
-- >>> sequenceAOf (both . traversed) ([Just "apples"], [Nothing])
-- Nothing

-----------------------------
-- Infix `traverseOf`: %%∼ --
-----------------------------

-- |
-- >>> import Text.Read (readMaybe)
-- >>> (("1", "2") & both %%~ readMaybe) ∷ Maybe (Int, Int)
-- Just (1,2)

-- |
-- >>> import Text.Read (readMaybe)
-- >>> (("not a number", "2") & both %%~ readMaybe) ∷ Maybe (Int, Int)
-- Nothing

-------------------------------
-- Using Traversals Directly --
-------------------------------

-- Instead of using `traverseOf` or `%%∼,` we can often just use the traversal itself!

-- |
-- Here we use `both` directly as though it were `traverse`:
-- >>> import Text.Read (readMaybe)
-- >>> both readMaybe ("1", "2") ∷ Maybe (Int, Int)
-- Just (1,2)

-- I'd recommend avoiding this style; it's unnecessarily confusing, unidiomatic, and doesn't translate well to other optics libraries, but it's sometimes useful to know that it exists.
-- The combinators help a lot with readability, so I'd recommend you use them consistently.

-----------------------
-- Custom traversals --
-----------------------

---------------------------------
-- Optics Look like `traverse` --
---------------------------------

-- The symmetry we see in these shapes is why we can compose optics of differing types together.
-- Every optic is actually the exact same type plus or minus constraints on <f>!
-- As optics are composed, the constraints on <f> are gathered up.

--------------------------------
-- Our First Custom Traversal --
--------------------------------

-- Van Laarhoven optics are just a function which matches a ‘traverse-like’ signature we can write our own traversals by hand.

-- Author recommends fully expanding the type signature.
values ∷ Applicative f ⇒ (a → f b) → [a] → f [b]
values _ [] = pure []
values handler (a : as) = (:) <$> handler a <*> values handler as

-- values handler (a : as) = liftA2 (:) (handler a) (values handler as)

-- |
-- >>> ["one", "two", "three"] ^.. values
-- ["one","two","three"]

-- |
-- >>> ["one", "two", "three"] & values %~ reverse
-- ["eno","owt","eerht"]

-- |
-- We should be able to do type-changing transformations too:
-- >>> ["one", "two", "three"] & values %~ length
-- [3,3,5]

----------------------------------
-- Traversals with Custom Logic --
----------------------------------

data Transaction
  = Withdrawal {_moneyAmount ∷ Int}
  | Deposit {_moneyAmount ∷ Int}
  deriving (Show)

makeLenses ''Transaction

newtype BankAccount = BankAccount
  { _transactions ∷ [Transaction]
  }
  deriving (Show)

makeLenses ''BankAccount

-- |
-- Get all transactions
-- >>> let aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]
-- >>> aliceAccount ^.. transactions . traversed
-- [Deposit {_moneyAmount = 100},Withdrawal {_moneyAmount = 20},Withdrawal {_moneyAmount = 10}]

-- |
-- Get the amounts for all transactions
-- >>> let aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]
-- >>> aliceAccount ^.. transactions . traversed . moneyAmount
-- [100,20,10]

---------------------------------------
-- Case Study: Transaction Traversal --
---------------------------------------

-- The handler focuses elements, pure ignores them.
deposits ∷ Applicative f ⇒ (Int → f Int) → [Transaction] → f [Transaction]
deposits _ [] = pure []
deposits handler (Withdrawal amt : rest) = (Withdrawal amt :) <$> deposits handler rest
deposits handler (Deposit amt : rest) = liftA2 (:) (Deposit <$> handler amt) (deposits handler rest)

-- |
-- Get all the Deposit transaction amounts:
-- >>> [Deposit 10, Withdrawal 20, Deposit 30] ^.. deposits
-- [10,30]

-- |
-- Multiply the amounts of all Deposits by 10
-- >>> [Deposit 10, Withdrawal 20, Deposit 30] & deposits *~ 10
-- [Deposit {_moneyAmount = 100},Withdrawal {_moneyAmount = 20},Deposit {_moneyAmount = 300}]

-- WARNING, when focusing a subset of a list like this our first thought is often to look at using a helper
-- like filter to implement the traversal; but you need to be careful! filter is a destructive operation,
-- it throws away any parts of the list which don't match.

isDeposit ∷ Transaction → Bool
isDeposit (Deposit _) = True
isDeposit _ = False

badDeposits ∷ Traversal' [Transaction] Int
badDeposits handler ts = traverse go (filter isDeposit ts)
  where
    go (Deposit amt) = Deposit <$> handler amt
    go (Withdrawal _) = error "This shouldn't happen"

-- |
-- >>> [Deposit 10, Withdrawal 20, Deposit 30] ^.. badDeposits
-- [10,30]

-- |
-- The `Withdrawal` is lost.
-- >>> [Deposit 10, Withdrawal 20, Deposit 30] & badDeposits *~ 10
-- [Deposit {_moneyAmount = 100},Deposit {_moneyAmount = 300}]

-- Using existing traversals.
deposits' ∷ Traversal' [Transaction] Int
deposits' = traversed . filtered isDeposit . moneyAmount

-- |
-- >>> [Deposit 10, Withdrawal 20, Deposit 30] ^.. deposits'
-- [10,30]

-- |
-- >>> [Deposit 10, Withdrawal 20, Deposit 30] & deposits' *~ 10
-- [Deposit {_moneyAmount = 100},Withdrawal {_moneyAmount = 20},Deposit {_moneyAmount = 300}]

--------------------
-- Traversal Laws --
--------------------

-----------------------------
-- Law One: Respect Purity --
-----------------------------

-- |
-- When we run a traversal, we expect it to run our handler on the focused elements, then thread the effects through to the outside, but do nothing else!
-- >>> (traverseOf both pure ("don't", "touch") ∷ [(String, String)]) == (pure ("don't", "touch") ∷ [(String, String)])
-- True

-- |
-- >>> (traverseOf both pure ("don't", "touch") ∷ Maybe (String, String)) == (pure ("don't", "touch") ∷ Maybe (String, String))
-- True
badTupleSnd ∷ Traversal (Int, a) (Int, b) a b
badTupleSnd handler (n, a) = (n + 1,) <$> handler a

-- The idea is that traversals shouldn't be messing around where they don't belong. Let the handlers do the updating and don't mess with state yourself!

-- |
-- >>> traverseOf badTupleSnd pure (10, "Yo")
-- (11,"Yo")

-- |
-- >>> pure (10, "Yo")
-- (10,"Yo")

---------------------------------
-- Law Two: Consistent Focuses --
---------------------------------

-- x & myTraversal %~ f
--   & myTraversal %~ g
-- ==
-- x & myTraversal %~ (g . f)

-- In essence this law states that the traversal should never change which elements it focuses due to alterations on those elements.
-- `both` is a law-abiding traversal, so we expect to see the equality hold for any structure or handlers we can dream up:

-- |
-- >>> ((0, 0) & both %~ (+10) & both %~ (*10)) == ((0, 0) & both %~ (*10) . (+10))
-- True

-- |
-- `filtered` is a law-breaking traversal.
-- >>> (2 & filtered even %~ (+1) & filtered even %~ (*10)) == (2 & filtered even %~ (*10) . (+1))
-- False

-- Good Traversal Bad Traversal --

-- Why is there a traversal included in the lens library that blatantly breaks one of the laws?
-- As it turns out, the traversal laws are more guidelines than anything.

---------------------------
-- Advanced Manipulation --
---------------------------

-- partsOf ∷ Traversal' s a → Lens' s [a]

-- The lens generated by partsOf takes all the focuses of the provided traversal and packs them into a
-- list for you to manipulate however you like. Then, it takes the modified list and maps each element
-- back into the original structure!!!
-- We're welcome to use ANY list manipulation functions we want!

-- |
-- >>> [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _1)
-- "abc"

-- |
-- >>> [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _2)
-- [1,2,3]

-- |
-- We can `set` the lens to a list to replace the corresponding elements
-- >>> [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) .~ ['c', 'a', 't']
-- [('c',1),('a',2),('t',3)]

-- |
-- Any 'extra' list elements are simply ignored
-- >>> [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) .~ ['l', 'e', 'o', 'p', 'a', 'r', 'd']
-- [('l',1),('e',2),('o',3)]

-- |
-- Providing too few elements will keep the originals
-- >>> [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) .~ ['x']
-- [('x',1),('b',2),('c',3)]

-- |
-- >>> [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) %~ reverse
-- [('c',1),('b',2),('a',3)]

-- |
-- >>> [('o', 1), ('o', 2), ('f', 3)] & partsOf (traversed . _1) %~ sort
-- [('f',1),('o',2),('o',3)]

-- |
-- See if you can follow along with how this one works:
-- >>> [('o', 1), ('o', 2), ('f', 3)] & partsOf (traversed . _1) %~ tail
-- [('o',1),('f',2),('f',3)]

-- |
-- >>> ("how is a raven ", "like a ", "writing desk") & partsOf (each . traversed) %~ unwords . sort . words
-- ("a a desk how is"," like r","aven writing")

-- ("how is a raven ", "like a ", "writing desk")
-- 'each' collects each string
-- "how is a raven " "like a " "writing desk"
-- 'traversed' focuses each individual character
-- 'h' 'o' 'w' ' ' 'i' 's' ...
-- The 'partsOf' around (each . traversed) collects the focuses into a list:
-- "how is a raven like a writing desk"
-- 'words' splits the list into words
-- ["how","is","a","raven","like","a","writing","desk"]
-- 'sort' sorts the words
-- ["a","a","desk","how","is","like","raven","writing"]
-- unwords flattens back to a String (a.k.a. list of characters)
-- "a a desk how is like raven writing"
-- 'partsOf' then maps each character back to a position
-- a a desk how is like r aven writing"
-- Result:
-- ("a a desk how is"," like r","aven writing")

-- |
-- Collect 'each' tuple element into a list, then traverse that list
-- >>> ("abc", "def") ^.. partsOf each . traversed
-- ["abc","def"]

-- |
-- Collect each tuple element, then traverse those strings collecting each character into a list.
-- >>> ("abc", "def") ^.. partsOf (each . traversed)
-- ["abcdef"]

-- |
-- You can use partsOf to edit elements using their neighbours as context.
-- >>> [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _2) %~ \xs → (/ sum xs) <$> xs
-- [('a',0.16666666666666666),('b',0.3333333333333333),('c',0.5)]

-- [('a', 1), ('b', 2), ('c', 3)]
-- Focus each number with 'partsOf (traversed . _2)'
-- [1, 2, 3]
-- Substitute into the function:
-- (/ sum [1, 2, 3]) <$> [1, 2, 3]
-- Evaluate
-- [0.16666,0.33333,0.5]
-- partsOf maps back each element into the original structure
-- 0.16666 0.33333 0.5
-- Result:
-- [('a',0.16666), ('b',0.33333), ('c',0.5)]

-------------------------
-- Polymorphic partsOf --
-------------------------

-- |
-- [('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) .~ [True, False]
-- unsafePartsOf': not enough elements were supplied

-- |
-- >>> [('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) .~ [True, False, True]
-- [(True,1),(False,2),(True,3)]

-- |
-- >>> [('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) %~ \xs → zipWith (,) xs ((Just <$> tail xs) ++ [Nothing])
-- [(('a',Just 'b'),1),(('b',Just 'c'),2),(('c',Nothing),3)]

--------------------------------------------------------------------------------------------
--                               Indexable Structures                                     --
--------------------------------------------------------------------------------------------

--------------------------------------
-- What's an *indexable* structure? --
--------------------------------------

-- |
-- Getting values at an index uses !!
-- >>> let xs = ["Henry I", "Henry II", "Henry III"]
-- >>> xs !! 0
-- "Henry I"

-- |
-- >>> let xs = ["Henry I", "Henry II", "Henry III"]
-- >>> xs !! 1
-- "Henry II"

-- |
-- let xs = ["Henry I", "Henry II", "Henry III"]
-- xs !! 3
-- Prelude.!!: index too large

-- |
-- Maps use `lookup` to 'get' values at an index
-- >>> let turtles = M.fromList [("Leo", "Katanas"), ("Raph", "Sai")]
-- >>> M.lookup "Leo" turtles
-- Just "Katanas"

-- |
-- Maps use `adjust` to update the value at an index
-- >>> let turtles = M.fromList [("Leo", "Katanas"), ("Raph", "Sai")]
-- >>> M.adjust ("Two " <>) "Leo" turtles
-- fromList [("Leo","Two Katanas"),("Raph","Sai")]

-----------------------------------------------
-- Accessing and Updating Values with 'Ixed' --
-----------------------------------------------

-- Type families are functions that operate on types!

-- The index for lists is `Int`:
-- type instance Index [a] = Int
-- type instance IxValue [a] = a

-- The index for Maps is the key type:
-- type instance Index (Map k a) = k
-- type instance IxValue (Map k a) = a

--------------------------------------------
-- Accessing and Setting Values with `ix` --
--------------------------------------------

-- |
-- Get the value at index 1:
-- >>> let humanoids = ["Borg", "Cardassian", "Talaxian"]
-- >>> humanoids ^? ix 1
-- Just "Cardassian"

-- Just "Cardassian"

-- |
-- There's no value at index 10 so the traversal doesn't focus anything.
-- >>> let humanoids = ["Borg", "Cardassian", "Talaxian"]
-- >>> humanoids ^? ix 10
-- Nothing

-- |
-- It's a traversal, so we can `set` new values at that index.
-- >>> let humanoids = ["Borg", "Cardassian", "Talaxian"]
-- >>> humanoids & ix 1 .~ "Vulcan"
-- ["Borg","Vulcan","Talaxian"]

-- |
-- A `set` will do nothing if the given index doesn't have a value.
-- >>> let humanoids = ["Borg", "Cardassian", "Talaxian"]
-- >>> humanoids & ix 10 .~ "Romulan"
-- ["Borg","Cardassian","Talaxian"]

-- |
-- Get the value at key "Zuko".
-- >>> let benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> benders ^? ix "Zuko"
-- Just "Fire"

-- |
-- If there's no value at a key, the traversal returns zero elements.
-- >>> let benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> benders ^? ix "Sokka"
-- Nothing

-- |
-- We can set the value at a key, but only if that key already exists.
-- >>> let benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> benders & ix "Toph" .~ "Metal"
-- fromList [("Katara","Water"),("Toph","Metal"),("Zuko","Fire")]

-- |
-- Setting a non-existent element of a Map does NOT insert it.
-- >>> let benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> benders & ix "Iroh" .~ "Lightning"
-- fromList [("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]

------------------------
-- Indexed Structures --
------------------------

-- Structure   | Index  | Value
-- ------------|--------|-------
-- [a]         | Int    | a
-- NonEmpty a  | Int    | a
-- Seq a       | Int    | a
-- Vector a    | Int    | a
-- Set a       | a      | ()
-- Map k a     | k      | a
-- Identity a  | ()     | a
-- Maybe a     | ()     | a
-- Tree a      | [Int]  | a
-- Text        | Int    | Char
-- ByteString  | Int    | Word8
-- (e → a)     | e      | a

--------------------------------
-- Indexing Monomorphic Types --
--------------------------------

-- |
-- ("hello" ∷ Text) ^? ix 0
-- Just 'h'

-- |
-- ("hello" ∷ Text) ^? ix 10
-- Nothing

-- |
-- ("hello" ∷ Text) & ix 0 .~ 'j'
-- "jello"

-- |
-- Word8's are shown as integers
-- ("hello" ∷ ByteString) ^? ix 0
-- Just 104

-- |
-- ("hello" ∷ ByteString) ^? ix 10
-- Nothing

-- |
-- We can edit a Word8 within a ByteString as though it's an integer.
-- ("hello" ∷ ByteString) & ix 0 +~ 2
-- "jello"

-- |
-- Remember, we can always 'traverse' a traversal using effectful handlers!
-- ("hello" ∷ Text) & ix 1 %%~ \_ → ("aeiou" ∷ [Char])
-- [ "hallo" , "hello" , "hillo" , "hollo" , "hullo" ]

----------------------------------
-- Indexing Stranger Structures --
----------------------------------

--     1
--    / \
--   2   3
--  /   / \
-- 4   5   6

tree ∷ Tree Int
tree = Node 1 [Node 2 [Node 4 []], Node 3 [Node 5 [], Node 6 []]]

-- |
-- The empty list represents the value at the root:
-- >>> tree ^? ix []
-- Just 1

-- |
-- 0 represents the first child of the root.
-- >>> tree ^? ix [0]
-- Just 2

-- |
-- 0, 0 descends into the first child twice then focuses the value at that node.
-- >>> tree ^? ix [0, 0]
-- Just 4

-- |
-- >>> tree ^? ix [1, 0]
-- Just 5

-- |
-- >>> tree ^? ix [1, 1]
-- Just 6

-- |
-- Invalid paths simply return an empty traversal.
-- >>> tree ^? ix [5, 6]
-- Nothing

-- |
-- Invalid paths simply return an empty traversal.
-- Indexing into a function runs the function with the index as input
-- >>> reverse ^? ix "Stella!"
-- Just "!alletS"

-- |
-- Invalid paths simply return an empty traversal.
-- We can set or traverse individual results of a function!
-- Here we overwrite the function's output at the input value "password" so it instead returns a new value.
-- >>> let specialReverse = reverse & ix "password" .~ "You found the secret!"
-- >>> specialReverse "password"
-- "You found the secret!"

-- |
-- The function is unaffected at all other inputs
-- >>> let specialReverse = reverse & ix "password" .~ "You found the secret!"
-- >>> specialReverse "dunno"
-- "onnud"

------------------------------------
-- Inserting & Deleting with `At` --
------------------------------------

-------------------------
-- Map-like Structures --
-------------------------

-- |
-- >>> let turtles = M.fromList [("Leo", "Katanas"), ("Raph", "Sai")]
-- >>> M.insert "Mikey" "Nunchaku" turtles
-- fromList [("Leo","Katanas"),("Mikey","Nunchaku"),("Raph","Sai")]

-- |
-- >>> let turtles = M.fromList [("Leo", "Katanas"), ("Raph", "Sai")]
-- >>> M.delete "Leo" turtles
-- fromList [("Raph","Sai")]

-- To insert or replace an element we can set a value wrapped in Just; to delete we can set the focus to Nothing.
-- We can update a value arbitrarily using over and providing a function from `Maybe a → Maybe a`.

-- For comparison, here's ix and at side-by-side:
-- ix ∷ Index m → Traversal' m (IxValue m)
-- at ∷ Index m → Lens'      m (Maybe (IxValue m))

-- |
-- Since 'at' creates a lens, we use `^.` instead of `^?`
-- >>> let benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> benders ^. at "Zuko"
-- Just "Fire"

-- |
-- >>> let benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> benders & at "Zuko" .~ Nothing
-- fromList [("Katara","Water"),("Toph","Earth")]

-- |
-- >>> let benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> benders & at "Iroh" .~ Just "Lightning"
-- fromList [("Iroh","Lightning"),("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]

-- |
-- Using the (?~) operator we can avoid writing out the Just when inserting elements.
-- >>> let benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> benders & at "Iroh" ?~ "Lightning"
-- fromList [("Iroh","Lightning"),("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]

-- |
-- We can use this with other optics if we want, but it's usually used with `at`
-- >>> (1, 2) & both ?~ "twins!"
-- (Just "twins!",Just "twins!")

-- |
-- `sans` is just short-hand for setting the value at an index to `Nothing`.
-- >>> let benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> sans "Katara" benders
-- fromList [("Toph","Earth"),("Zuko","Fire")]

-----------------------
-- Manipulating Sets --
-----------------------

-- One way to imagine a Set is as a map where the set elements are keys: (Map v ()).
-- Using unit: () as the value means the only real information stored in the Map is whether a value exists or not

primes ∷ Set Int
primes = S.fromList [2, 3, 5, 7, 11, 13]

-- |
-- >>> primes ^? ix 5
-- Just ()

-- |
-- >>> primes ^? ix 4
-- Nothing

-- |
-- >>> primes & at 17 ?~ ()
-- fromList [2,3,5,7,11,13,17]

-- |
-- >>> sans 5 primes
-- fromList [2,3,7,11,13]

-- |
-- We can use `&` to chain uses of `sans`.
-- >>> primes & sans 5 & sans 7 & sans 11
-- fromList [2,3,13]

------------------------------------
-- Custom Indexed Data Structures --
------------------------------------

newtype Cycled a = Cycled [a]
  deriving (Show)

type instance Index (Cycled a) = Int

type instance IxValue (Cycled a) = a

instance Ixed (Cycled a) where
  ix ∷ Applicative f ⇒ Int → (a → f a) → Cycled a → f (Cycled a)
  ix i handler (Cycled xs) = Cycled <$> traverseOf (ix (i `mod` length xs)) handler xs

-- |
-- >>> Cycled ['a', 'b', 'c'] ^? ix 1
-- Just 'b'

-- |
-- >>> Cycled ['a', 'b', 'c'] ^? ix 3
-- Just 'a'

-- |
-- >>> Cycled ['a', 'b', 'c'] ^? ix 10
-- Just 'b'

-- |
-- >>> Cycled ['a', 'b', 'c'] ^? ix (-1)
-- Just 'c'

-- |
-- >>> Cycled ['a', 'b', 'c'] & ix 0 .~ '!'
-- Cycled "!bc"

-- |
-- >>> Cycled ['a', 'b', 'c'] & ix 10 .~ '!'
-- Cycled "a!c"

-- |
-- >>> Cycled ['a', 'b', 'c'] & ix (-1) .~ '!'
-- Cycled "ab!"

---------------------------------
-- Custom At: Address Indexing --
---------------------------------

data PostalAddress = PostalAddress
  { _buildingNumber ∷ Maybe String,
    _streetsName ∷ Maybe String,
    _apartmentNumber ∷ Maybe String,
    _postalCode ∷ Maybe String
  }
  deriving (Show)

makeLenses ''PostalAddress

data AddressPiece
  = BuildingNumber
  | StreetName
  | ApartmentNumber
  | PostalCode
  deriving (Show)

type instance Index PostalAddress = AddressPiece

type instance IxValue PostalAddress = String

-- We need the instance declaration for Ixed, but can leave the implementation blank.
-- It will be implemented automatically in terms of `At`
instance Ixed PostalAddress

instance At PostalAddress where
  at ∷ AddressPiece → Lens' PostalAddress (Maybe String)
  at BuildingNumber = buildingNumber
  at StreetName = streetsName
  at ApartmentNumber = apartmentNumber
  at PostalCode = postalCode

-- |
-- >>> let addr = PostalAddress Nothing Nothing Nothing Nothing
-- >>> addr
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Nothing, _apartmentNumber = Nothing, _postalCode = Nothing}

-- |
-- >>> let addr = PostalAddress Nothing Nothing Nothing Nothing
-- >>> let sherlockAddr = addr & at StreetName ?~ "Baker St." & at ApartmentNumber ?~ "221B"
-- >>> sherlockAddr
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Just "Baker St.", _apartmentNumber = Just "221B", _postalCode = Nothing}

-- |
-- >>> let addr = PostalAddress Nothing Nothing Nothing Nothing
-- >>> let sherlockAddr = addr & at StreetName ?~ "Baker St." & at ApartmentNumber ?~ "221B"
-- >>> sherlockAddr & ix ApartmentNumber .~ "221A"
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Just "Baker St.", _apartmentNumber = Just "221A", _postalCode = Nothing}

-- |
-- >>> let addr = PostalAddress Nothing Nothing Nothing Nothing
-- >>> let sherlockAddr = addr & at StreetName ?~ "Baker St." & at ApartmentNumber ?~ "221B"
-- >>> sherlockAddr & sans StreetName
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Nothing, _apartmentNumber = Just "221B", _postalCode = Nothing}

-----------------------------
-- Handling Missing Values --
-----------------------------

--------------------------------------
-- Checking whether Updates Succeed --
--------------------------------------

-- |
-- There's no element at index 6, so the update fails
-- >>> "abcd" & failover (ix 6) toUpper ∷ Maybe String
-- Nothing

-- |
-- There's an element at index 2, so the update succeeds!
-- >>> "abcd" & failover (ix 2) toUpper ∷ Maybe String
-- Just "abCd"

-- |
-- >>> [] & failover _head (*10) ∷ Maybe [Int]
-- Nothing

-- |
-- We can nest traversals, the whole chain fails if it focuses no elements
-- >>> (M.fromList[('a', [1, 2])] & failover (ix 'a' . ix 3) (*10) ∷ Maybe (M.Map Char [Int]))
-- Nothing

-- |
-- We can nest traversals, the whole chain fails if it focuses no elements
-- >>> (M.fromList[('a', [1, 2])] & failover (ix 'a' . ix 1) (*10) ∷ Maybe (M.Map Char [Int]))
-- Just (fromList [('a',[1,20])])

-- |
-- It even works with filters
-- >>> [1, 3, 5] & failover (traversed . filtered even) (*10) ∷ Maybe [Int]
-- Nothing

-- |
-- It even works with filters
-- >>> [1, 3, 5] & failover (traversed . filtered odd) (*10) ∷ Maybe [Int]
-- Just [10,30,50]

-- |
-- First update will succeed.
-- >>> let s = "abcdefg"
-- >>> failover (ix 8) toUpper s <|> failover (ix 6) toUpper s <|> failover (ix 4) toUpper s
-- "abcdefG"

------------------------------
-- Fallbacks with `failing` --
------------------------------

-- |
-- Try to get something from index 10, failing that, get something from index 2.
-- >>> M.fromList [('a', 1), ('b', 2)] ^? (ix 'z' `failing` ix 'b')
-- Just 2

-- |
-- It works with updates as well:
-- >>> M.fromList [('a', 1), ('b', 2)] & (ix 'z' `failing` ix 'b') *~ 10
-- fromList [('a',1),('b',20)]

-- |
-- Get the first album available in the map in order of preference.
-- >>> M.fromList [("Bieber" ∷ String, "Believe"), ("Beyoncé", "Lemonade")] ^? (ix "Swift" `failing` ix "Bieber" `failing` ix "Beyoncé")
-- Just "Believe"

-- |
-- The optics we pass to `failing` can be arbitrarily complex so long as the type of the focus is the same for each argument.
-- >>> M.fromList [('a', (1, [2, 3, 4])), ('b', (5, [6, 7, 8]))] ^.. (ix 'z' . _1 `failing` ix 'a' . _2 . ix 10 `failing` ix 'b' . _2 . traversed)
-- [6,7,8]

----------------------
-- Default Elements --
----------------------

-- This mimics the behaviour of `fromMaybe` using optics.

-- |
-- >>> Nothing ^. non 0
-- 0

-- |
-- >>> Nothing ^. non "default"
-- "default"

-- |
-- >>> Just "value" ^. non "default"
-- "value"

-- |
-- Using `non` unwraps the `Maybe` so we can view values directly.
-- "Leslie" has a favourite food, so we can look it up:
-- >>> let favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
-- >>> favouriteFoods ^. at "Leslie" . non "Pizza"
-- "Waffles"

-- |
-- If we don't know someone's favourite food, the default is "Pizza"
-- >>> let favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
-- >>> favouriteFoods ^. at "Leo" . non "Pizza"
-- "Pizza"

-- |
-- When setting through `non` it allows us to set values directly without worrying about wrapping them in `Just`.
-- >>> let favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
-- >>> favouriteFoods & at "Popeye" . non "Pizza" .~ "Spinach"
-- fromList [("Garfield","Lasagna"),("Leslie","Waffles"),("Popeye","Spinach")]

-- |
-- Define an alias for our optic with the default value included.
-- "Garfield" isn't stored when his favourite matches the default.
-- >>> let fav name = at name . non "Pizza"
-- >>> let favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
-- >>> let newFavourites = favouriteFoods & fav "Garfield" .~ "Pizza"
-- >>> newFavourites
-- fromList [("Leslie","Waffles")]

-- |
-- We still get the correct value when retrieving Garfield's favourite.
-- >>> let fav name = at name . non "Pizza"
-- >>> let favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
-- >>> let newFavourites = favouriteFoods & fav "Garfield" .~ "Pizza"
-- >>> newFavourites ^. fav "Garfield"
-- "Pizza"

-- Modifying values.

-- |
-- Erin will be added to the map since she's missing.
-- >>> M.fromList [("Jim", 32), ("Dwight", 39)] & at "Erin" . non 0 +~ 10
-- fromList [("Dwight",39),("Erin",10),("Jim",32)]

-- |
-- Since Jim already has hours logged we simply increment them.
-- >>> M.fromList [("Jim", 32), ("Dwight", 39)] & at "Jim" . non 0 +~ 8
-- fromList [("Dwight",39),("Jim",40)]

-- |
-- >>> M.fromList [("Jim", 32), ("Dwight", 39)] & at "Dwight" . non 0 .~ 11
-- fromList [("Dwight",11),("Jim",32)]

-- |
-- When we pay-out an employee's hours, we set their hours to `0`
-- `non` removes any keys with the default value from the list entirely.
-- >>> M.fromList [("Jim", 32), ("Dwight", 39)] & at "Dwight" . non 0 .~ 0
-- fromList [("Jim",32)]

-----------------------------------
-- Checking Fold Success/Failure --
-----------------------------------

-- |
-- >>> fromMaybe 'z' ("abc" ^? ix 1)
-- 'b'

-- |
-- >>> fromMaybe 'z' ("abc" ^? ix 10)
-- 'z'

-- `pre` is a version of `preview` as a higher-order optic. You pass it a fold and it will try to get a value from it, returning `Nothing` if it can't find one.
-- Note that it returns a Getter, so we can't set or update using `pre`.

-- |
-- >>> "abc" ^. pre (ix 10) . non 'z'
-- 'z'

-- |
-- We use ^. rather than ^? since `pre` turns the fold into a Getter.
-- >>> [1, 2, 3, 4] ^. pre (traversed . filtered even)
-- Just 2

-- |
-- >>> [1, 3] ^. pre (traversed . filtered even)
-- Nothing

-- |
-- We can combine this with `non` and `ix` to get default values when accessing list elements:
-- >>> "abc" ^. pre (ix 20) . non 'z'
-- 'z'

--------------------------------------------------------------------------------------------
--                                         Prisms                                         --
--------------------------------------------------------------------------------------------

-------------------------------------------
-- How do Prisms Fit into the Hierarchy? --
-------------------------------------------

--            | Get      | Set/Modify | Traverse | Embed
---------------------------------------------------------
-- Lens       |  Single  |   Single   | Single   |   ✗
-- Fold       |   Many   |     ✗      |   ✗      |   ✗
-- Traversal  |   Many   |   Many     | Many     |   ✗
-- Prism      | Zero/One |  Zero/One  | Zero/One |  One

-- Prisms can be run backwards, taking a focus and embedding it into a structure!
-- We'll see how prisms are a natural candidate for specifying pattern-matching semantics and help to work with sum-types as well.

------------------------------------
-- Simple Pattern-Matching Prisms --
------------------------------------

-- For the `Either` type we have `_Left` and `_Right` prisms

-- |
-- >>> Left "message" ^? _Left
-- Just "message"

-- |
-- >>> Left "message" ^? _Right
-- Nothing

-- |
-- >>> Right 42 ^? _Right
-- Just 42

-- |
-- >>> Right 42 ^? _Left
-- Nothing

-- |
-- Since prisms are valid traversals we can set, update, or traverse the focused value through them:
-- >>> Left 10 & _Left +~ 5
-- Left 15

-- |
-- >>> Right "howdy" & _Right %~ reverse
-- Right "ydwoh"

-- |
-- Same for `Maybe`.
-- >>> Nothing ^? _Nothing
-- Just ()

-- |
-- >>> Nothing ^? _Just
-- Nothing

-- |
-- >>> Just "gold" ^? _Just
-- Just "gold"

-- |
-- >>> Just "gold" ^? _Nothing
-- Nothing

-- |
-- >>> Just 20 & _Just %~ (+10)
-- Just 30

-----------------------------------------------------------
-- Checking Pattern Matches with Prisms (`has`, `isn't`) --
-----------------------------------------------------------

-- |
-- >>> has _Right (Left "message")
-- False

-- |
-- >>> has _Right (Right 37)
-- True

-- |
-- >>> isn't _Nothing (Just 12)
-- True

-- |
-- >>> isn't _Left (Left ())
-- False

-----------------------------------------
-- Generating Prisms with `makePrisms` --
-----------------------------------------

-- A path is a list of URL segments
type Path = [String]

type Body = String

data Request
  = Post Path Body
  | Get Path
  | Delete Path
  deriving (Show)

-- Creates `_Post` `_Get` and `_Delete` prisms.
makePrisms ''Request

-- This is all we need to use these prisms for getting or setting as if they were traversals:

-- |
-- >>> Get ["users"] ^? _Get
-- Just ["users"]

-- |
-- >>> Get ["users"] ^? _Post
-- Nothing

-- |
-- >>> Get ["users"] & _Get .~ ["posts"]
-- Get ["posts"]

-- |
-- >>> Post ["users"] "name: John" ^? _Post
-- Just (["users"],"name: John")

-- |
-- >>> Post ["users"] "name: John" & _Post . _1 <>~ ["12345"]
-- Post ["users","12345"] "name: John"

----------------------------------
-- Embedding Values with Prisms --
----------------------------------

-- Every prism represents a pattern-match which can be *reversed*.
-- By feeding a prism a focus we can embed that focus into a structure via the context implied by the pattern.
-- Even though viewing through a prism may fail if the pattern doesn't match, embedding a value into a pattern using a prism always succeeds!
-- To run a prism backwards we use the `review` action or its infix version (#); which you can think of as short for "reverse view".
-- It embeds the focus into the prism's pattern.

-- |
-- Prisms which match on a constructor of some type can be reversed to embed fields into the constructor.
-- The reverse of unpacking a specific constructor is to pack those fields into that constructor.
-- >>> review _Get ["posts"]
-- Get ["posts"]

-- |
-- We can use the infix alias to accomplish the same:
-- >>> _Get # ["posts"]
-- Get ["posts"]

-- |
-- >>> _Delete # ["posts"]
-- Delete ["posts"]

-- |
-- Constructors with multiple fields accept a tuple of the fields.
-- >>> _Post # (["posts"], "My blog post")
-- Post ["posts"] "My blog post"

-- |
-- Construct a `Left` from a string
-- >>> review _Left "an error"
-- Left "an error"

-- |
-- >>> review _Right 42
-- Right 42

-- |
-- composing prisms
-- >>> _Just . _Left # 1337
-- Just (Left 1337)

-----------------------------
-- Other Types of Patterns --
-----------------------------

-- Although most of the prisms you'll encounter will be used for matching on data type constructors, prisms can also encode more complex and abstract patterns.
-- Unlike regular pattern matching if a prism fails to match it won't crash, instead the prism simply won't focus anything.

-- |
-- >>> [1, 2, 3] ^? _Cons
-- Just (1,[2,3])

-- |
-- >>> "Freedom!" ^? _Cons
-- Just ('F',"reedom!")

-- |
-- >>> "" ^? _Cons
-- Nothing

-- |
-- >>> "Freedom!" & _Cons . _2 %~ reverse
-- "F!modeer"

-- |
-- `_Cons` is a prism, so we can run it backwards using `review` (a.k.a. `#`), to cons an element onto the front of a list-like type.
-- This operation will never fail!
-- >>> _Cons # ('F', "reedom")
-- "Freedom"

-- |
-- >>> "Freedom!" & _tail %~ reverse
-- "F!modeer"

-- |
-- >>> "Hello" & _head .~ 'J'
-- "Jello"

-- |
-- >>> "" & _head .~ 'J'
-- ""

-- |
-- >>> isn't _Empty []
-- False

-- |
-- >>> isn't _Empty [1, 2, 3]
-- True

-- |
-- The phrasing for 'has' isn't quite as clear Feel free to simply define 'is = has' if that helps.
-- >>> has _Empty M.empty
-- True

-- |
-- >>> has _Empty (S.fromList [1, 2, 3])
-- False

-- The `_Show` prism can Read or Show values to and from their String representations.
-- The "pattern" we're matching on is whether the value can successfully be parsed into the result type (which will be determined by type inference).
-- If the string fails to parse into the output type properly the prism will not match.
-- To run it in reverse it calls "show" on the provided value to turn it back into a string.

-- |
-- >>> "12" ^? _Show ∷ Maybe Int
-- Just 12

-- |
-- The type we assert is important. If we pick a different type it changes the behaviour.
-- >>> "12" ^? _Show ∷ Maybe Bool
-- Nothing

-- |
-- >>> "apple pie" ^? _Show ∷ Maybe Int
-- Nothing

-- |
-- Get a list of all Integers in a sentence!
-- >>> "It's True that I ate 3 apples and 5 oranges" ^.. worded . _Show ∷ [Int]
-- [3,5]

-- |
-- Make sure you specify the correct output type.
-- `_Show` uses the output type to decide what to try to read.
-- Changing the expected type can even change the result!
-- >>> "It's True that I ate 3 apples and 5 oranges" ^.. worded . _Show ∷ [Bool]
-- [True]

---------------------------
-- Writing Custom Prisms --
---------------------------

_Just' ∷ Prism (Maybe a) (Maybe b) a b
_Just' = prism embed match
  where
    embed ∷ b → Maybe b
    embed b = Just b

    match ∷ Maybe a → Either (Maybe b) a
    match (Just a) = Right a
    match Nothing = Left Nothing

-- Since the _Nothing prism doesn’t focus the type variable we know it can’t be a polymorphic prism.
-- This means we can use the simpler prism' helper in this case.
_Nothing' ∷ Prism' (Maybe a) ()
_Nothing' = prism' embed match
  where
    embed ∷ () → Maybe a
    embed () = Nothing

    match ∷ Maybe a → Maybe ()
    match Nothing = Just ()
    match (Just _) = Nothing

------------------------------
-- Matching String Prefixes --
------------------------------

_Prefix ∷ String → Prism' String String
_Prefix prefix = prism' embed match
  where
    embed ∷ String → String
    embed s = prefix <> s

    match ∷ String → Maybe String
    match s = stripPrefix prefix s

-- |
-- >>> "http://phishingscam.com" ^? _Prefix "https://"
-- Nothing

-- |
-- >>> "https://totallylegit.com" ^? _Prefix "https://"
-- Just "totallylegit.com"

-- |
-- We can even define new prisms using our existing one.
-- Only add our account number if the connection is secure!
-- >>> let _Secure = _Prefix "https://"
-- >>> "https://mybank.com" & _Secure <>~ "?accountNumber=12345"
-- "https://mybank.com?accountNumber=12345"

-- |
-- >>> let _Secure = _Prefix "https://"
-- >>> "http://fakebank.com" & _Secure <>~ "?accountNumber=12345"
-- "http://fakebank.com"

--------------------------------------------------
-- Cracking the Coding Interview: Prisms Style! --
--------------------------------------------------

_Factor ∷ Int → Prism' Int Int
_Factor n = prism' embed match
  where
    embed ∷ Int → Int
    embed i = i * n

    match ∷ Int → Maybe Int
    match i
      | i `mod` n == 0 = Just (i `div` n)
      | otherwise = Nothing

-- |
-- Is 3 a factor of 9?
-- >>> has (_Factor 3) 9
-- True

-- |
-- Is 7 NOT a factor of 9?
-- >>> isn't (_Factor 7) 9
-- True

-- |
-- We can factor 5 out of 15 to get 3;
-- 3 * 5 == 15
-- >>> 15 ^? _Factor 5
-- Just 3

-- |
-- 15 is not evenly divisible by 7
-- >>> 15 ^? _Factor 7
-- Nothing

-- Notice how we ‘divide out’ the number when we match. This allows us to properly chain our matches and discover more factors!

-- |
-- >>> 15 ^? _Factor 3 . _Factor 5
-- Just 1

-- |
-- >>> 30 ^? _Factor 5 . _Factor 3
-- Just 2

-- FizzBuzz with Prisms
prismFizzBuzz ∷ Int → String
prismFizzBuzz n
  | has (_Factor 3 . _Factor 5) n = "FizzBuzz"
  | has (_Factor 3) n = "Fizz"
  | has (_Factor 5) n = "Buzz"
  | otherwise = show n

runFizzBuzz ∷ IO ()
runFizzBuzz = for_ [1 .. 20] $ \n → putStrLn (prismFizzBuzz n)

-- |
-- runFizzBuzz

----------
-- Laws --
----------

-----------------------------
-- Law One: Review-Preview --
-----------------------------

-- preview p (review p value) == Just value

-- |
-- >>> preview _Left (review _Left "Jabberwocky") == Just "Jabberwocky"
-- True

-- |
-- >>> preview _Just (review _Just "Jabberwocky") == Just "Jabberwocky"
-- True

-------------------------------
-- Law Two: Prism Complement --
-------------------------------

-- If we preview with a matching prism to get the focus 'a'
-- let Just a = preview myPrism s

-- Then we 'review' that focus 'a'
-- let s' = review myPrism a

-- We must end up back where we started
-- s == s'

-- |
-- >>> let s = Right 32
-- >>> let Just a = preview _Right s
-- >>> let s' = review _Right a
-- >>> s == s'
-- True

-- |
-- `_Show` is unlawful.
-- >>> let s = "[1, 2, 3]"
-- >>> let Just a = preview (_Show ∷ Prism' String [Int]) s
-- >>> let s' = review _Show a
-- >>> s == s'
-- False

-- |
-- Note the different spacing:
-- >>> let s = "[1, 2, 3]"
-- >>> s
-- "[1, 2, 3]"

-- |
-- _Show has normalized the value to have no spaces between values
-- >>> let s = "[1, 2, 3]"
-- >>> let Just a = preview (_Show ∷ Prism' String [Int]) s
-- >>> let s' = review _Show a
-- >>> s'
-- "[1,2,3]"

---------------------------------------
-- Law Three: Pass-through Reversion --
---------------------------------------

-- let Left t = matching l s
-- let Left s' = matching l t
-- s == s'

-- |
-- >>> let s = Nothing ∷ Maybe Int
-- >>> let Left (t ∷ Maybe String) = matching _Just s
-- >>> let Left (s' ∷ Maybe Int) = matching _Just t
-- >>> s == s'
-- True

-- |
-- >>> let s = Left "Yo" ∷ Either String Int
-- >>> let Left (t ∷ Either String Bool) = matching _Right s
-- >>> let Left (s' ∷ Either String Int) = matching _Right t
-- >>> s == s'
-- True

-------------------------------
-- Case Study: Simple Server --
-------------------------------

path ∷ Lens' Request Path
path = lens getter setter
  where
    getter (Post p body) = p
    getter (Get p) = p
    getter (Delete p) = p
    setter (Post _ body) p = Post p body
    setter (Get _) p = Get p
    setter (Delete _) p = Delete p

-- |
-- >>> Get ["posts", "12345"] ^. path
-- ["posts","12345"]

-- |
-- >>> Post ["posts", "12345"] "My new post" ^. path
-- ["posts","12345"]

-- |
-- >>> Get ["posts", "12345"] & path .~ ["hello"]
-- Get ["hello"]

-- Default server handler:
serveRequest ∷ Request → String
serveRequest _ = "404 Not Found"

-- |
-- >>> serveRequest (Get ["hello"])
-- "404 Not Found"

-- |
-- >>> serveRequest (Delete ["user"])
-- "404 Not Found"

-- |
-- >>> serveRequest (Post ["hello"] "Is anyone there?")
-- "404 Not Found"

--------------------------
-- Path prefix matching --
--------------------------

_PathPrefix ∷ String → Prism' Request Request
_PathPrefix prefix = prism' embed match
  where
    -- Add the prefix to the path
    embed ∷ Request → Request
    embed req = req & path %~ (prefix :)

    -- Check if the prefix matches the path
    match ∷ Request → Maybe Request
    match req | has (path . _head . only prefix) req = Just (req & path %~ drop 1)
    match _ = Nothing

-- |
-- >>> (Get ["users", "all"]) ^? _PathPrefix "users"
-- Just (Get ["all"])

-- |
-- >>> (Delete ["posts", "12345"]) ^? _PathPrefix "posts"
-- Just (Delete ["12345"])

-- |
-- Fails to match when prefixes differ
-- >>> (Get ["other"]) ^? _PathPrefix "users"
-- Nothing

-- |
-- Can we run it backwards?
-- >>> _PathPrefix "users" # Get ["all"]
-- Get ["users","all"]

-- |
-- >>> _PathPrefix "posts" # Delete ["12345"]
-- Delete ["posts","12345"]

-- NOTE!!! We could have implemented _PathPrefix much easier using the existing prefix prism from the lens library, but I wanted to show how it works behind the scenes.

------------------------------------
-- Altering Sub-Sets of Functions --
------------------------------------

-- |
-- >>> tail [1, 2, 3]
-- [2,3]

-- |
-- >>> tail []
-- *** Exception: Prelude.tail: empty list

-- We pass in the original tail function as the value we’re modifying, then we focus the portion of the
-- function which deals with the empty list by using outside _Empty. We replace that portion of the
-- function with one that always returns [].
safeTail ∷ [a] → [a]
safeTail = tail & outside _Empty .~ const []

-- |
-- >>> safeTail [1, 2, 3]
-- [2,3]

-- |
-- >>> safeTail []
-- []

-- Handlers
userHandler ∷ Request → String
userHandler req = "User handler! Remaining path: " <> intercalate "/" (req ^. path)

postsHandler ∷ Request → String
postsHandler = const "Posts Handler!" & outside (_PathPrefix "index") .~ const "Post Index"

server ∷ Request → String
server =
  serveRequest & outside (_PathPrefix "users") .~ userHandler
    & outside (_PathPrefix "posts") .~ postsHandler

-- |
-- >>> server (Get ["users", "12345"])
-- "User handler! Remaining path: 12345"

-- |
-- >>> server (Delete ["users", "12345", "profile"])
-- "User handler! Remaining path: 12345/profile"

-- |
-- It should run the default handler for non user paths.
-- >>> server (Post ["admin"] "DROP TABLE users")
-- "404 Not Found"

-- |
-- >>> server (Get ["posts"])
-- "Posts Handler!"

-- |
-- >>> server (Get ["posts", "index"])
-- "Post Index"

---------------------------
-- Matching on HTTP Verb --
---------------------------

postsHandler' ∷ Request → String
postsHandler' =
  const "404 Not Found"
    & outside _Post .~ (\(path', body) → "Created post with body: " <> body)
    & outside _Get .~ (\path' → "Fetching post at path: " <> intercalate "/" path')
    & outside _Delete .~ (\path' → "Deleting post at path: " <> intercalate "/" path')

server' ∷ Request → String
server' =
  serveRequest & outside (_PathPrefix "users") .~ userHandler
    & outside (_PathPrefix "posts") .~ postsHandler'

-- |
-- >>> server' (Get ["posts", "12345"])
-- "Fetching post at path: 12345"

-- |
-- >>> server' (Post ["posts", "12345"] "My new post")
-- "Created post with body: My new post"

-- |
-- >>> server' (Delete ["posts", "12345"])
-- "Deleting post at path: 12345"

--------------------------------------------------------------------------------------------
--                                          Isos                                          --
--------------------------------------------------------------------------------------------

-----------------------------------------
-- How do Isos Fit into the Hierarchy? --
-----------------------------------------

--            | Get      | Set/Modify | Traverse | Embed
---------------------------------------------------------
-- Lens       |  Single  |   Single   | Single   |   ✗
-- Fold       |   Many   |     ✗      |   ✗      |   ✗
-- Traversal  |   Many   |   Many     | Many     |   ✗
-- Prism      | Zero/One |  Zero/One  | Zero/One |   ✓
-- Iso        |  Single  |   Single   | Single   |   ✓

-- Because Isos are completely reversible they’re the strongest of all the optics we’ve seen.
-- By strongest I actually mean most constrained.
-- Every Iso MUST succeed for all inputs, and MUST be completely reversible.
-- These strong constraints let us do a lot with them!
-- Because of their strength, Isos are a valid substitution for any other type of optic we've learned.

-- can be used as | Lens | Fold | Traversal
-- -------------- | -----| -----| ----------
-- Lens           |  ✓   |  ✓   |    ✓
-- Fold           |  ✗   |  ✓   |    ✗
-- Traversal      |  ✗   |  ✓   |    ✓
-- Iso            |  ✓   |  ✓   |    ✓

--------------------------
-- There and Back Again --
--------------------------

-- uncurry . curry == id
-- curry . uncurry == id

-- uncurry . curry == curry . uncurry

-- T.pack . T.unpack == id
-- T.unpack . T.pack == id

-------------------
-- Building Isos --
-------------------

-- An iso is an optic which views data after running its transformation on it.
-- We can view, modify or traverse the data in it's altered form!
-- If we modify the data in its altered form, the iso will convert the result back through the iso into the original form.
-- E.g. we can use an iso to edit a String as though it were a Text, and end up with a String again after the modification.

-- We convert the data through the Iso, run the modification, then convert it back.

packed ∷ Iso' String Text
packed = iso to' from'
  where
    to' ∷ String → Text
    to' = T.pack

    from' ∷ Text → String
    from' = T.unpack

-- |
-- Using `packed` views Strings as though they were Text.
-- >>> "Ay, caramba!" ^. packed
-- "Ay, caramba!"

-- |
-- `review`ing an iso runs its inverse
-- packed # ("Sufferin' Succotash" ∷ Text)
-- "Sufferin' Succotash" ∷ String

-------------------------------
-- Flipping Isos with `from` --
-------------------------------

-- |
-- >>> ("Good grief" ∷ String) ^. packed
-- "Good grief"

-- |
-- ("Good grief" ∷ Text) ^. from packed
-- "Good grief" ∷ String
unpacked ∷ Iso' Text String
unpacked = from packed

-- Note: Both `packed` and `unpacked` are available in Data.Text.Lens from the `lens` library.

------------------------------------
-- Modification under Isomorphism --
------------------------------------

-- |
-- Using the awesome `replace` function from Text.
-- let str = "Idol on a pedestal" ∷ String
-- str & packed %~ T.replace "Idol" "Sand"
-- "Sand on a pedestal"

-- |
-- Using `over` in place of `%~` also reads nicely.
-- let str = "Idol on a pedestal" ∷ String
-- over packed (T.replace "Idol" "Sand") str
-- "Sand on a pedestal"

-- |
-- We can of course compose Isos with other optics!
-- We could just use `T.toUpper` instead, but this demonstrates the point:
-- let txt = "Lorem ipsum" ∷ Text
-- txt & from packed . traversed %~ toUpper
-- "LOREM IPSUM"

-------------------------------
-- Varieties of Isomorphisms --
-------------------------------

reversed' ∷ Iso' [a] [a]
reversed' = iso reverse reverse

reversed'' ∷ Iso' [a] [a]
reversed'' = involuted reverse

-- |
-- >>> reverse . reverse $ [1, 2, 3]
-- [1,2,3]

-- |
-- >>> [1, 2, 3] & reversed'' %~ drop 1
-- [1,2]

-- |
-- >>> [1, 2, 3] & reversed'' %~ take 1
-- [3]

-- |
-- We gain a lot of power by combining isos with all the other combinators we've learned.
-- >>> [1, 2, 3, 4] ^.. reversed'' . takingWhile (> 2) traversed
-- [4,3]

-- |
-- We can reverse more than once! But you probably shouldn't...
-- >>> "Blue suede shoes" & reversed'' . taking 1 worded . reversed'' .~ "gloves"
-- "Blue suede gloves"

-- |
-- swapped lets us view a tuple as though the slots were swapped around.
-- >>> ("Fall","Pride") ^. swapped
-- ("Pride","Fall")

-- |
-- >>> Right "Field" ^. swapped
-- Left "Field"

-- |
-- >>> let (++?) = (++) ^. flipped
-- >>> "A" ++? "B"
-- "BA"

-- |
-- >>> let addTuple = (+) ^. uncurried
-- >>> addTuple (1, 2)
-- 3

-- |
-- >>> 10 ^. negated
-- -10

-- |
-- >>> over negated (+10) 30
-- 20

-- |
-- >>> 100 ^. adding 50
-- 150

-- |
-- Be careful of division by 0 for 'dividing' and 'multiplying'
-- >>> 100.0 ^. dividing 10
-- 10.0

--------------------
-- Composing Isos --
--------------------

-- Isos compose just like any other optic! They compose both the ‘forwards’ AND ‘reversed’ transformations.

-- |
-- let txt = "Winter is coming" ∷ Text
-- txt ^. unpacked . reversed
-- "gnimoc si retniW"

-- |
-- let txt = "Winter is coming" ∷ Text
-- txt & unpacked . reversed %~ takeWhile (not . isSpace)
-- "coming"

{- ORMOLU_DISABLE -}

--          INPUT                               OUTPUT
--          =====                               ======
--
-- ("Winter is coming" ∷ Text)             ("coming" ∷ Text)
--           |                                      |
-- +---------v------------------------+----------------+-----------------+
-- |      unpacked                    |        from unpacked             |
-- +---------+------------------------+-------------^--------------------+
--           |                                      |
--    ("Winter is coming" ∷ String)         ("coming" ∷ String)
--           |                                      |
-- +---------v------------------------+-------------+--------------------+
-- |     reversed                     |      from reversed               |
-- +---------+------------------------+-------------^--------------------+
--           |                                      |
--  "gnimoc si retniW"                          "gnimoc"
--           |                                      |
--           +-----→ takeWhile (not . isSpace) >----+

{- ORMOLU_ENABLE -}

-- |
-- >>> 30.0 & dividing 10 . multiplying 2 +~ 1
-- 35.0

-- The inverse of division is multiplication, and the inverse of multiplication is division.

{- ORMOLU_DISABLE -}

--         INPUT                         OUTPUT
--         =====                         ======
--
--          30.0                          35.0
--           |                             |
-- +---------v---------------+----------------+-----------------+
-- |      dividing 10        |        from (dividing 10)        |
-- +---------+---------------+-------------^--------------------+
--           |                             |
--          3,0                           3,5
--           |                             |
-- +---------v---------------+-------------+--------------------+
-- |     multiplying 2       |      from (multiplying 2)        |
-- +---------+---------------+-------------^--------------------+
--           |                             |
--          6.0                           7.0
--           |                             |
--           +-----→       6 + 1      >----+

{- ORMOLU_ENABLE -}

---------------------
-- Projecting Isos --
---------------------

-- `mappings` let's us easily transform nested portions of structures in our optics path as we go along!

toYamlList ∷ [String] → String
toYamlList xs = "- " <> intercalate "\n- " xs

-- |
-- >>> toYamlList ["Milk", "Eggs","Flour"]
-- "- Milk\n- Eggs\n- Flour"

-- |
-- let shoppingList = ["Milk", "Eggs","Flour"] ∷ [Text]
-- let strShoppingList = shoppingList ^. mapping unpacked ∷ [String]
-- toYamlList strShoppingList
-- "- Milk\n- Eggs\n- Flour"

-- |
-- Or we can do it all in one go!
-- let shoppingList = ["Milk", "Eggs","Flour"] ∷ [Text]
-- shoppingList ^. mapping unpacked . to toYamlList
-- "- Milk\n- Eggs\n- Flour"

-- |
-- We can even use `traverseOf_`!
-- let shoppingList = ["Milk", "Eggs","Flour"] ∷ [Text]
-- traverseOf_ (mapping unpacked . to toYamlList) putStrLn $ shoppingList
-- "- Milk\n- Eggs\n- Flour"

-- We can lift isos through functors? That applies to contravariant functors, bifunctors and even profunctors too!
-- The functions are called: `contramapping`, `bimapping`, `dimapping`.

-- We can contramap over the input and map over the output all in one go using dimapping!
-- We take the argument, map and unpack, then run our function. Lastly we pack the functions result.
textToYamlList ∷ [Text] → Text
textToYamlList = toYamlList ^. dimapping (mapping unpacked) packed

-- This is the simple version. :-)
textToYamlList' ∷ [Text] → Text
textToYamlList' = T.pack . toYamlList . fmap T.unpack

-----------------------
-- Isos and Newtypes --
-----------------------

------------------------
-- Coercing with Isos --
------------------------

-- import Data.Coerce (coerce)
-- :t coerce

-- coerce ∷ Coercible a b ⇒ a → b

-- Coercible is a special constraint, it’s implemented for us by GHC for any newtype, we can use it even though it won’t show up in instance lists.

newtype Email = Email String
  deriving (Show)

newtype UserID = UserID String
  deriving (Show)

-- Each of these types are representationally equal to Strings; the only difference is the type!

-- |
-- We need to specify which type to coerce into
-- >>> coerce ("joe@example.com" ∷ String) ∷ Email
-- Email "joe@example.com"

-- If two types are representationally equal we can even skip the middle-man and go directly between two newtypes:
-- >>> coerce (Email "joe@example.com") ∷ UserID
-- UserID "joe@example.com"

-- |
-- >>> over coerced (reverse ∷ String → String) (Email "joe@example.com") ∷ Email
-- Email "moc.elpmaxe@eoj"

-- |
-- Type inference really struggles, and we'll almost always need to add a lot of extra annotations.
-- >>> Email "joe@example.com" & (coerced ∷ Iso' Email String) . traversed %~ toUpper
-- Email "JOE@EXAMPLE.COM"

-- using a little helper
email ∷ Iso' Email String
email = coerced

-- |
-- This is much better!
-- >>> Email "joe@example.com" & email . traversed %~ toUpper
-- Email "JOE@EXAMPLE.COM"
newtype Mail = Mail {_mail ∷ String}
  deriving (Show)

makeLenses ''Mail

-- `makeLenses` will generate a mail iso which behaves just like the one we would have manually defined!

-- |
-- >>> Mail "joe@example.com" & mail . traversed %~ toUpper
-- Mail {_mail = "JOE@EXAMPLE.COM"}

--------------------------
-- Newtype Wrapper Isos --
--------------------------

-- Alternative is to use `makeWrapped` which creates _Wrapped', _Unwrapped' and _Wrapping'.
-- makeWrapped ''Mail

----------
-- Laws --
----------

-----------------------------------------
-- The One and Only Law: Reversibility --
-----------------------------------------

-- myIso . from myIso == id
-- from myIso . myIso == id

-- `id` is a valid optic which always focuses its whole argument, it’s a valid Iso as well!

-- |
-- >>> view (reversed . from reversed) ("Testing one two three")
-- "Testing one two three"

-- |
-- >>> view (from reversed . reversed) ("Testing one two three")
-- "Testing one two three"

-- |
-- >>> view (negated . from negated) 23
-- 23

-- |
-- >>> view (from negated . negated) 23
-- 23

-- Composition of Isos is also an Iso.
myIso ∷ Iso' Double Double
myIso = negated . adding 10.0 . multiplying 372.0

-- |
-- >>> view (myIso . from myIso) 23.0
-- 23.0

-- |
-- It's not perfect, but close enough. :-)
-- >>> view (from myIso . myIso) 23.0
-- 23.000000000000227

--------------------------------------------------------------------------------------------
--                                     Indexed Optics                                     --
--------------------------------------------------------------------------------------------

------------------------------
-- What are Indexed Optics? --
------------------------------

-- The basic idea of indexed optics is that they allow you to accumulate information about your current focus as you dive deeper into an optics path.

-- |
-- >>> toListOf itraversed ["Summer", "Fall", "Winter", "Spring"]
-- ["Summer","Fall","Winter","Spring"]

-- |
-- >>> itoListOf itraversed ["Summer", "Fall", "Winter", "Spring"]
-- [(0,"Summer"),(1,"Fall"),(2,"Winter"),(3,"Spring")]

-- action     | operator | indexed action | indexed operator
-- -----------+----------+----------------+------------------
-- toListOf   |   (^..)  |   itoListOf    |    (^@..)
-- over       |   (%∼)   |   iover        |    (%@∼)
-- traverseOf |   (%%∼)  |   itraverseOf  |    (%%@∼)
-- set        |   (.∼)   |   iset         |    (.@∼)
-- view       |   (^.)   |   iview        |    (^@.)

-- |
-- The index type of maps is the key, so we can get a list of all elements and their key:
-- >>> let agenda = M.fromList [("Monday", "Shopping"), ("Tuesday", "Swimming")]
-- >>> agenda ^@.. itraversed
-- [("Monday","Shopping"),("Tuesday","Swimming")]

-- |
-- The index type of tuples is the first half of the tuple:
-- >>> (True, "value") ^@.. itraversed
-- [(True,"value")]

-- |
-- The index type of trees is a list of int's which indicates their location in the tree.
-- >>> let t = Node "top" [Node "left" [], Node "right" []]
-- >>> t ^@.. itraversed
-- [([],"top"),([0],"left"),([1],"right")]

-- |
-- By default, the index of a path will be the index of the last optic in the path.
-- >>> let agenda = M.fromList [ ("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"]) ]
-- >>> agenda ^@.. itraversed . itraversed
-- [(0,"Shopping"),(1,"Yoga"),(0,"Brunch"),(1,"Food coma")]

-- |
-- If we end the path with a non-indexed optic (like traverse) we’ll get an error:
-- When using with indexed actions make sure that your path has the type you expect and ends with an indexed optic!
-- let agenda = M.fromList [ ("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"]) ]
-- agenda ^@.. itraversed . traverse
-- Couldn't match type ‘Indexed i a (Const (Endo [(i, a)]) a)’
--                with ‘[Char] → Const (Endo [(i, a)]) [Char]’
-- Expected type: IndexedGetting
--                  i (Endo [(i, a)]) (Map [Char] [[Char]]) a
--   Actual type: ([Char] → Const (Endo [(i, a)]) [Char])
--                → Map [Char] [[Char]]
--                → Const (Endo [(i, a)]) (Map [Char] [[Char]])

-- index-aware composition operators:
--   • (<.): Use the index of the optic to the left
--   • (.>): Use the index of the optic to the right (This is how . already behaves)
--   • (<.>): Combine the indices of both sides as a tuple

-- |
-- >>> let agenda = M.fromList [ ("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"]) ]
-- >>> agenda ^@.. itraversed <. itraversed
-- [("Monday","Shopping"),("Monday","Yoga"),("Saturday","Brunch"),("Saturday","Food coma")]

-- |
-- We can keep both by using (<.>).
-- >>> let agenda = M.fromList [ ("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"]) ]
-- >>> agenda ^@.. itraversed <.> itraversed
-- [(("Monday",0),"Shopping"),(("Monday",1),"Yoga"),(("Saturday",0),"Brunch"),(("Saturday",1),"Food coma")]

-- |
-- Unlike normal (.), (<.>) is NOT associative, re-associating will change the way the tuples nest.
-- This is a ridiculous thing to do, but it'll show how the indexes nest,
-- We'll use an indexed traversal over the characters of each activity name:
-- >>> let agenda = M.fromList [ ("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"]) ]
-- >>> agenda ^@.. itraversed <.> itraversed <.> itraversed
-- [(("Monday",(0,0)),'S'),(("Monday",(0,1)),'h'),(("Monday",(0,2)),'o'),(("Monday",(0,3)),'p'),(("Monday",(0,4)),'p'),(("Monday",(0,5)),'i'),(("Monday",(0,6)),'n'),(("Monday",(0,7)),'g'),(("Monday",(1,0)),'Y'),(("Monday",(1,1)),'o'),(("Monday",(1,2)),'g'),(("Monday",(1,3)),'a'),(("Saturday",(0,0)),'B'),(("Saturday",(0,1)),'r'),(("Saturday",(0,2)),'u'),(("Saturday",(0,3)),'n'),(("Saturday",(0,4)),'c'),(("Saturday",(0,5)),'h'),(("Saturday",(1,0)),'F'),(("Saturday",(1,1)),'o'),(("Saturday",(1,2)),'o'),(("Saturday",(1,3)),'d'),(("Saturday",(1,4)),' '),(("Saturday",(1,5)),'c'),(("Saturday",(1,6)),'o'),(("Saturday",(1,7)),'m'),(("Saturday",(1,8)),'a')]

------------------------------
-- Custom Index Composition --
------------------------------

-- We could use `icompose` to append the list index to the day of the week as a string:
showDayAndNumber ∷ String → Int → String
showDayAndNumber a b = a <> ": " <> show b

-- |
-- >>> let agenda = M.fromList [ ("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"]) ]
-- >>> agenda ^@.. icompose showDayAndNumber itraversed itraversed
-- [("Monday: 0","Shopping"),("Monday: 1","Yoga"),("Saturday: 0","Brunch"),("Saturday: 1","Food coma")]

-- Let's define a custom optics composition operator that will automatically append String indices with a comma separator.
(.++) ∷ (Indexed String s t → r) → (Indexed String a b → s → t) → Indexed String a b → r
(.++) = icompose (\a b → a ++ ", " ++ b)

populationMap ∷ Map String (Map String Int)
populationMap = M.fromList [("Canada", M.fromList [("Ottawa", 994837), ("Toronto", 2930000)]), ("Germany", M.fromList [("Berlin", 3748000), ("Munich", 1456000)])]

-- |
-- >>> populationMap ^@.. itraversed .++ itraversed
-- [("Canada, Ottawa",994837),("Canada, Toronto",2930000),("Germany, Berlin",3748000),("Germany, Munich",1456000)]

------------------------
-- Filtering by Index --
------------------------

-- |
-- Get list elements with an 'even' list-index:
-- >>> ['a'..'z'] ^.. itraversed . indices even
-- "acegikmoqsuwy"

-- |
-- >>> let ratings = M.fromList [ ("Dark Knight", 94) , ("Dark Knight Rises", 87) , ("Death of Superman", 92)]
-- >>> ratings ^.. itraversed . indices (has (prefixed "Dark"))
-- [94,87]

-- |
-- `index` works similarly to `indices`, but ignores any focus that doesn't have the exact index you specify.
-- >>> ['a'..'z'] ^? itraversed . index 10
-- Just 'k'

-- |
-- >>> let ratings = M.fromList [ ("Dark Knight", 94) , ("Dark Knight Rises", 87) , ("Death of Superman", 92)]
-- >>> ratings ^? itraversed . index "Death of Superman"
-- Just 92

---------------------------
-- Custom Indexed Optics --
---------------------------

{- ORMOLU_DISABLE -}

data Board a
  = Board
      a a a
      a a a
      a a a
  deriving (Show, Foldable)

data Position = I | II | III
  deriving (Show, Eq, Ord)

testBoard ∷ Board Char
testBoard =
  Board
    'X' 'O' 'X'
    '.' 'X' 'O'
    '.' 'O' 'X'

{- ORMOLU_ENABLE -}

-------------------------
-- Custom IndexedFolds --
-------------------------

-- Use a list comprehension to get the list of all coordinate pairs
-- in the correct order, then zip them with all the slots in our board
slotsFold ∷ IndexedFold (Position, Position) (Board a) a
slotsFold = ifolding $ \board → zip [(x, y) | y ← [I, II, III], x ← [I, II, III]] (toList board)

-- |
-- >>> testBoard ^@.. slotsFold
-- [((I,I),'X'),((II,I),'O'),((III,I),'X'),((I,II),'.'),((II,II),'X'),((III,II),'O'),((I,III),'.'),((II,III),'O'),((III,III),'X')]

-- |
-- Filter indices where the Y coord is 'II'
-- >>> testBoard ^@.. slotsFold . indices ((== II) . snd)
-- [((I,II),'.'),((II,II),'X'),((III,II),'O')]

------------------------------
-- Custom IndexedTraversals --
------------------------------

{- ORMOLU_DISABLE -}

-- I define a polymorphic indexed traversal
-- with a tuple of positions as the index:
slotsTraversal ∷ IndexedTraversal (Position, Position) (Board a) (Board b) a b
slotsTraversal p (Board a1 b1 c1
                        a2 b2 c2
                        a3 b3 c3) =
  Board
    <$> indexed p (I, I) a1
    <*> indexed p (II, I) b1
    <*> indexed p (III, I) c1
    <*> indexed p (I, II) a2
    <*> indexed p (II, II) b2
    <*> indexed p (III, II) c2
    <*> indexed p (I, III) a3
    <*> indexed p (II, III) b3
    <*> indexed p (III, III) c3

{- ORMOLU_ENABLE -}

-- |
-- Every traversal is also a fold.
-- >>> testBoard ^@.. slotsTraversal
-- [((I,I),'X'),((II,I),'O'),((III,I),'X'),((I,II),'.'),((II,II),'X'),((III,II),'O'),((I,III),'.'),((II,III),'O'),((III,III),'X')]

-- |
-- Setting the whole second row to 'O'.
-- >>> testBoard & slotsTraversal . indices ((== II) . snd) .~ 'O'
-- Board 'X' 'O' 'X' 'O' 'O' 'O' '.' 'O' 'X'

-- We pass the coordinates to our printing function so we can easily add newlines in the right spots!
printBoard ∷ Board Char → IO ()
printBoard = itraverseOf_ slotsTraversal printSlot
  where
    printSlot (III, _) c = putStrLn [c]
    printSlot (_, _) c = putStr [c]

-- |
-- printBoard testBoard
-- XOX
-- .XO
-- .OX

-- You can also write an indexed lens using the ilens helper; unlike the others it’s pretty straight-forward:
--
-- ilens ∷ (s → (i, a)) → (s → b → t) → IndexedLens i s t a b
--
-- You simply provide the index type alongside the focus in your getter and ilens will wire it up correctly!

-------------------
-- Index Helpers --
-------------------

-- |
-- The indexing helper takes a normal optic and simply adds a numeric index alongside its elements.
-- ("hello" ∷ Text) ^@.. indexing each
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]

-- |
-- We can re-map or edit the indexes of an optic using reindexed:
-- >>> ['a'..'c'] ^@.. itraversed
-- [(0,'a'),(1,'b'),(2,'c')]

-- |
-- >>> ['a'..'c'] ^@.. reindexed (*10) itraversed
-- [(0,'a'),(10,'b'),(20,'c')]

-- |
-- We can even change index types!
-- >>> ['a'..'c'] ^@.. reindexed show itraversed
-- [("0",'a'),("1",'b'),("2",'c')]

-- `selfIndex` can be injected into a path to set the index of the path to the current value.

-- |
-- `selfIndex` copies a snapshot of the current focus into the index
-- >>> [("Betty", 37), ("Veronica", 12)] ^@.. itraversed . selfIndex <. _2
-- [(("Betty",37),37),(("Veronica",12),12)]

-----------------------------
-- Index-Preserving Optics --
-----------------------------

-- Index preserving optics are just regular optics which pass-through any existing index in the path.

-- |
-- This one won't compile!
-- We require an indexed optic since we're using `^@..`, but `_1` 'forgets' the index from `itraversed` without adding any index of its own.
-- [('a', True), ('b', False), ('c', True)] ^@.. itraversed . _1
-- Couldn't match type ‘Indexed i a (Const (Endo [(i, a)]) a)’
--                with ‘Char → Const (Endo [(i, a)]) Char’
-- Expected type: IndexedGetting i (Endo [(i, a)]) [(Char, Bool)] a
--   Actual type: (Char → Const (Endo [(i, a)]) Char)
--                → [(Char, Bool)] → Const (Endo [(i, a)]) [(Char, Bool)]

-- |
-- However, we can turn `_1` into an index preserving lens!
-- Now the index 'passes-through' `_1'` to the end.
-- >>> let _1' = cloneIndexPreservingLens _1
-- >>> [('a', True), ('b', False), ('c', True)] ^@.. itraversed . _1'
-- [(0,'a'),(1,'b'),(2,'c')]

-- |
-- The previous is equivalent to use explicit index passing with `<.`
-- >>> [('a', True), ('b', False), ('c', True)] ^@.. itraversed <. _1
-- [(0,'a'),(1,'b'),(2,'c')]

-- cloneIndexPreserving* methods: `cloneIndexPreservingLens`, `cloneIndexPreservingTraversal`, `cloneIndexPreservingSetter`

-- `iplens` is identical to `lens` except the resulting lens will be index-preserving.
-- iplens ∷ (s → a) → (s → b → t) → IndexPreservingLens s t a b

--------------------------------------------------------------------------------------------
--                                 Dealing with Type Errors                               --
--------------------------------------------------------------------------------------------

-- no sample code

--------------------------------------------------------------------------------------------
--                                    Optics and Monads                                   --
--------------------------------------------------------------------------------------------

---------------------------
-- Reader Monad and View --
---------------------------
type BenutzerName = String

type Password = String

data Env = Env
  { _currentUser ∷ BenutzerName,
    _users ∷ Map BenutzerName Password
  }
  deriving (Show)

makeLenses ''Env

printUser ∷ ReaderT Env IO ()
printUser = do
  user ← asks _currentUser
  liftIO . putStrLn $ "Current user: " <> user

-- The equivalent lensy version uses `view` like this.
-- This is actually the exact same view function we're used to using with lenses!
printUser' ∷ ReaderT Env IO ()
printUser' = do
  user ← view currentUser
  liftIO . putStrLn $ "Current user: " <> user

-- `preview` works the same way!
getUserPassword ∷ ReaderT Env IO ()
getUserPassword = do
  userName ← view currentUser
  maybePassword ← preview (users . ix userName)
  liftIO $ print maybePassword

-- see main function for application!

-----------------------------
-- State Monad Combinators --
-----------------------------

data Till = Till
  { _total ∷ Double,
    _sales ∷ [Double],
    _taxRate ∷ Double
  }
  deriving (Show)

makeLenses ''Till

-- Almost ALL setter combinators have State equivalents which simply replace the `∼` with an `=`!

saleCalculation ∷ StateT Till IO ()
saleCalculation = do
  total .= 0
  total += 8.55 -- Delicious Hazy IPA
  total += 7.36 -- Red Grapefruit Sour
  totalSale ← use total -- `use` is like `view`, but for MonadState rather than MonadReader!
  liftIO $ printf "Total sale: $%.2f\n" totalSale
  sales <>= [totalSale]
  total <~ uses taxRate (totalSale *) -- store (<~) it into our total using the total lens
  taxIncluded ← use total
  liftIO $ printf "Tax included: $%.2f\n" taxIncluded

-- |
-- execStateT saleCalculation (Till 0 [] 1.19)
-- Till {_total = 18.9329, _sales = [15.91], _taxRate = 1.19}

-- All of these MonadState combinators have alternate versions which return the existing or altered versions of the focus, see `(<+=)`, `(<<+=)`, `(<<∼)`, etc...

--------------------
-- Magnify & Zoom --
--------------------

data Weather = Weather
  { _temp ∷ Float,
    _pressure ∷ Float
  }
  deriving (Show)

makeLenses ''Weather

printData ∷ String → ReaderT Float IO ()
printData statName = do
  num ← ask
  liftIO . putStrLn $ statName <> ": " <> show num

weatherStats ∷ ReaderT Weather IO ()
weatherStats = do
  magnify temp (printData "temp") -- `magnify` et. al. allow us to ‘re-scope’ a Reader or State monad to a portion of the type focused by a lens.
  magnify pressure (printData "pressure")

-- |
-- runReaderT weatherStats (Weather 15 7.2)
-- temp: 15.0
-- pressure: 7.2

-- a State action which runs against our Weather object
convertCelsiusToFahrenheit ∷ StateT Float IO ()
convertCelsiusToFahrenheit = modify (\celsius → (celsius * (9 / 5)) + 32)

-- In order to run it in a State monad over the Weather type we'll need to zoom-in on the temperature when we run it.
weatherStats' ∷ StateT Weather IO ()
weatherStats' = zoom temp convertCelsiusToFahrenheit

-- |
-- >>> execStateT weatherStats' (Weather 32 12)
-- Weather {_temp = 89.6, _pressure = 12.0}

-------------------
-- Classy Lenses --
-------------------

-----------------------------------------------------
-- What are Classy Lenses and When Do I Need Them? --
-----------------------------------------------------

-- Classy lenses aren't really a new type of optic so much as a whole DESIGN PATTERN.

--------------------------------
-- No duplicate record fields --
--------------------------------

-- A consistent annoyance in Haskell is that you can’t have two records with the same field names.
-- Classy lenses provide an (imperfect) solution for this.

-- If we have several records which all have a `name` field, we need to disambiguate the fields.
-- The idiom is to use record name as a field prefix.
newtype Persona = Persona
  { _personaName ∷ String
  }
  deriving (Show)

newtype Pet = Pet
  { _petName ∷ String
  }
  deriving (Show)

-- Not only is this annoyingly verbose, but there's a greater problem in Haskell as a whole:
--   we can't write code which is polymorphic over record fields!
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

-- We can use `makeFields` to generate record-polymorphic lenses for all the fields.

makeFields ''Persona
makeFields ''Pet

-- `makeFields` creates a unified `HasName` class and instances for each of the records too:

-- :browse
-- class HasName s a | s → a where
--   name ∷ Lens' s a

-- :info HasName
-- instance HasName Person String
-- instance HasName Pet String

-- `HasName` is a FIELD (TYPE) CLASS, a typeclass which provides a lens we can use to get or set the value of that field for any type which implements the typeclass.

-- unify our two greeting functions.
greetByName ∷ HasName r String ⇒ r → String -- some type `r` has a `name` field of type String
greetByName r = "Hello " <> r ^. name <> "!"

-- |
-- >>> greetByName (Persona "Calvin")
-- "Hello Calvin!"

-- |
-- >>> greetByName (Pet "Hobbes")
-- "Hello Hobbes!"

------------------------------------------------------
-- Separating Logic and Minimizing Global Knowledge --
------------------------------------------------------

data DataDBEnv = DataDBEnv
  { _portNumber ∷ Int,
    _hostName ∷ String,
    _databaseUrl ∷ String
  }
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

main ∷ IO ()
main = do
  runReaderT printUser (Env "jenkins" (M.singleton "jenkins" "hunter2"))
  runReaderT printUser' (Env "jenkins" (M.singleton "jenkins" "hunter2"))
  runReaderT getUserPassword (Env "jenkins" (M.singleton "jenkins" "hunter2"))
  runReaderT getUserPassword (Env "jenkins" (M.singleton "jenkin" "hunter2"))
  flip runReaderT (DataDBEnv 8000 "example.com" "db.example.com") $ do
    initialize
    connectDB

---------------------------------------------
-- Granular Dependencies with `makeFields` --
---------------------------------------------

-- In case of a monad stack with a Reader this design pattern will make it much more handy.
-- See pages 282-284 for the actual code. Very helpful and interesting!

--------------------------------
-- Field Requirements Compose --
--------------------------------

-- A benefit of field typeclasses is that they’re specified with constraints, and constraints compose!
-- This means that if we want to accept a record with multiple specific fields we can do so easily by
-- just adding multiple Has* constraints.

-- See pages 284-285.

----------------------------------
-- `makeFields` vs `makeClassy` --
----------------------------------

-- There are a half-dozen different ways to generate lenses using TemplateHaskell so you'd certainly be forgiven for getting them confused.

-- See pages 285-289 outlines the differences between `makeFields` and `makeClassy`.
