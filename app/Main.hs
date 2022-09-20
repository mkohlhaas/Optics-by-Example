module Main where

import Control.Applicative (Applicative (liftA2), ZipList (ZipList), (<|>))
import Control.Arrow ((>>>))
import Control.Lens
import Control.Lens.Extras (biplate)
import Control.Lens.Unsound (lensProduct)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State
import Data.Bits.Lens (bitAt)
import Data.ByteString (ByteString)
import Data.Char (isAlpha, isNumber, isUpper, toLower, toUpper)
import Data.Coerce (coerce)
import Data.Either.Validation (Validation (..))
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.List (intercalate, nub, sort, sortOn, stripPrefix, transpose, elemIndex)
import qualified Data.List.NonEmpty as NE
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
-- Exercises - Optic Anatomy --
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
-- Exercises - Virtual Fields --
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

----------------------------------------
-- Introduction to Polymorphic Optics --
----------------------------------------

----------------------------------
-- Simple vs Polymorphic Optics --
----------------------------------

-- polymorphic lens
-- Lens s t a b

-- When we run an action (like over) on our lens we dive deep into the structure <s> to find the focus
-- <a>, then pass that to the action. The action has the option of modifying the focus to return a <b>,
-- then the lens glues everything back together and construct a <t>.

-- We need polymorphic lenses whenever an action might want to change the type of the focus!

-----------------------------------------
-- When do We Need Polymorphic Lenses? --
-----------------------------------------

---------------------------
-- Type-Changing Focuses --
---------------------------

-----------------------------------------------------
-- Changing type variables with polymorphic lenses --
-----------------------------------------------------

-- We can use polymorphic lenses to change the type of specific slots of a tuple's type,
-- but this principle generalizes to type variables in other data types as well.
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
-- >>> Promotion "A really delicious Peach" 25.0
-- Promotion {_item = "A really delicious Peach", _discountPercentage = 25.0}

-- |
-- let peachPromo = Promotion "A really delicious Peach" 25.0
-- :t peachPromo
-- peachPromo ∷ Promotion String

-- |
-- let buffyFigurines = ["Buffy", "Angel", "Willow", "Giles"]
-- :t buffyFigurines
-- buffyFigurines ∷ [String]

-- |
-- >>> let peachPromo = Promotion "A really delicious Peach" 25.0
-- >>> let buffyFigurines = ["Buffy", "Angel", "Willow", "Giles"]
-- >>> set item buffyFigurines peachPromo
-- Promotion {_item = ["Buffy","Angel","Willow","Giles"], _discountPercentage = 25.0}

-- |
-- let peachPromo = Promotion "A really delicious Peach" 25.0
-- let buffyFigurines = ["Buffy", "Angel", "Willow", "Giles"]
-- let buffyPromo = set item buffyFigurines peachPromo
-- :t buffyPromo
-- buffyPromo ∷ Promotion [String]

-- Can we write polymorphic Lenses for `best` and `worst`?
-- No. A Lens focuses always on one thing only.
-- You would need a Traversal.
data Preferences a = Preferences
  { _best ∷ a,
    _worst ∷ a
  }
  deriving (Show)

------------------------------------
-- Exercises - Polymorphic Lenses --
------------------------------------

-- 1. Write the type signature of the polymorphic lens which would allow changing a Vorpal x to a Vorpal y.
-- Lens (Vorpal x) (Vorpal y) x y

-- 2. Find one possible way to write a polymorphic lens which changes the type of the `best` and
--   `worst` fields in the `Preferences` type above. You're allowed to change the type of the lenses or
--    alter the type itself.

-- By pairing up both values in the focus we can pass BOTH values packed up into a tuple. We're still only
-- selecting one focus with the lens, but we’re being sneaky and filling that focus with TWO values. This
-- isn't terribly helpful in this case, but there are times when you may need to use this sort of roundabout method.
favourites ∷ Lens (Preferences a) (Preferences b) (a, a) (b, b)
favourites = lens getter setter
  where
    getter pref = (_best pref, _worst pref)
    setter pref (newBest, newWorst) = pref {_best = newBest, _worst = newWorst}

data Preferences' a b = Preferences'
  { _best' ∷ a,
    _worst' ∷ b
  }
  deriving (Show)

best' ∷ Lens (Preferences' a c) (Preferences' b c) a b
best' = lens getter setter
  where
    getter = _best'
    setter pref newVal = pref {_best' = newVal}

worst' ∷ Lens (Preferences' c a) (Preferences' c b) a b
worst' = lens getter setter
  where
    getter = _worst'
    setter pref newVal = pref {_worst' = newVal}

-- 3. We can change type of more complex types too. What is the type of a lens which could change the type variable here?
data Result e = Result {_lineNumber ∷ Int, _result ∷ Either e String}

-- Because the `e` is inside an Either type we can't focus it with a lens directly, we're not always
-- guaranteed to have an e available. We'll have to focus the entire Either and let the user decide
-- what to do if it's missing:
result ∷ Lens (Result e) (Result f) (Either e String) (Either f String)
result = lens getter setter
  where
    getter res@Result {_result} = _result
    setter res newEither = res {_result = newEither}

-- 4. It's thinking time! Is it possible to change more than one type variable at a time using a polymorphic lens?
-- Yes!

data ParseResult e a
  = Error e
  | ParseResult a
  deriving (Show)

--                   input getter
--                        |            output setter
--                        |                 |         output getter
--                        |                 |              |        input setter
--                        |                 |              |            |
parseResult ∷ Lens (ParseResult e a) (ParseResult f b) (Either e a) (Either f b)
parseResult = lens getter setter
  where
    getter (Error e) = Left e
    getter (ParseResult a) = Right a
    setter _ (Left e) = Error e
    setter _ (Right a) = ParseResult a

-- 5. BONUS Come up with some sort of lens to change from a Predicate a to a Predicate b

newtype Predicate a = Predicate (a → Bool)

-- This is another one of those cases where we can't focus the type variable directly, we'll need to pass
-- the full structure to the user and let them handle it:

pred ∷ Lens (Predicate a) (Predicate b) (a → Bool) (b → Bool)
pred = lens getter setter
  where
    getter (Predicate f) = f
    setter _ newFn = Predicate newFn

----------------------
-- Composing Lenses --
----------------------

------------------------------------------------------
-- How do I Update Fields in Deeply Nested Records? --
------------------------------------------------------

-- Deeply Nested Types
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

-- Using standard record update syntax to change street number. Very messy.
-- In most imperative languages this would be simply `sherlock.address.streetAddress.streetNumber = "221A"`!
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
-- Does it even work? Yes - at least something.
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

-- Terminology:
-- modifier ∷ (a → a)
-- updater ∷ (a → a) → (s → s)
-- e.g. updateAddress = updater, (Address → Address) = modifier
-- An updater receives and returns a modifier.
-- A modifier receives and returns the same type.

-- |
-- Composing two updaters creates a new updater:
-- :t (updateStreetAddress . updateStreetNumber)
-- (updateStreetAddress . updateStreetNumber) ∷ (String → String) → (Address → Address)

-- |
-- Composing three updaters.
-- :t (updateAddress . updateStreetAddress . updateStreetNumber)
-- (updateAddress . updateStreetAddress . updateStreetNumber) ∷ (String → String) → (Person → Person)

-- |
-- >>> (updateAddress . updateStreetAddress . updateStreetNumber) (const "221A") sherlock
-- Person {_fullName = "S. Holmes", _address = Address {_streetAddress = StreetAddress {_streetNumber = "221A", _streetName = "Baker Street"}, _city = "London", _country = "England"}}

----------------------
-- Composing Lenses --
----------------------

-- Updaters and the way we composed them together represent the essence of optics!
-- Composition is at the core of how optics work!
-- Just as composing updaters makes another updater, composing lenses makes a new lens!
-- "Lens composition" is actually just "function composition".

-- Comparing updaters and lenses:
--
-- updaters ∷             (String →   String) → Person →   Person
-- lenses   ∷ Functor f ⇒ (String → f String) → Person → f Person
-- Here's the simplified type alias signature of `lenses`:
--
-- The Functor is the magic that lets us run all sorts of interesting actions besides just updates.
--
-- lenses ∷ Lens' Person String

--------------------------------
-- How do Lens Types Compose? --
--------------------------------

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

-- Composition of Polymorphic Lenses
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

-- The path specialized to our specific types.
-- (_2 . material) ∷ Lens (Player, Item Wool) (Player, Item Sweater) Wool Sweater

-- Optics compose easily without much boiler-plate, so we should prefer many small precise optics rather than large bulky ones!

----------------------------------
-- Exercises - Lens Composition --
----------------------------------

-- 1. Fill in the blank with the appropriate composition of tuple lenses in the following statement:

-- >>> view ??? ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))
-- "Waldo"

-- |
-- >>> view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))
-- "Waldo"

-- 2. Given the following lens types, fill in the missing type of `mysteryDomino`.

-- fiveEightDomino ∷ Lens' Five Eight
-- mysteryDomino ∷ Lens' ??? ???
-- twoThreeDomino ∷ Lens' Two Three

-- dominoTrain ∷ Lens' Five Three
-- dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino

-- mysteryDomino ∷ Lens' Eight Two

-- 3. Using what you know about how lenses work under the hood; rewrite the following signature
-- as a polymorphic lens of the form: Lens s t a b. Then identify each animal as one of: pre-action
-- structure, post-action structure, pre-action focus, post-action focus.

-- Functor f ⇒ (Armadillo → f Hedgehog) → (Platypus → f BabySloth)

-- Lens Platypus BabySloth Armadillo Hedgehog

-- 4. Find a way to compose ALL of the following lensees together into one big path using each exactly once. What's the type of the resulting lens?

-- spuzorktrowmble   ∷ Lens Chumble      Spuzz      Gazork       Trowlg
-- gazorlglesnatchka ∷ Lens Gazork       Trowlg     Bandersnatch Yakka
-- zinkattumblezz    ∷ Lens Zink         Wattoom    Chumble      Spuzz
-- gruggazinkoom     ∷ Lens Grug         Pubbawup   Zink         Wattoom
-- banderyakoobog    ∷ Lens Bandersnatch Yakka      Foob         Mog
-- boowockugwup      ∷ Lens Boojum       Jabberwock Grug         Pubbawup
-- snajubjumwock     ∷ Lens Snark        JubJub     Boojum       Jabberwock

-- snajubjumwock     ∷ Lens Snark        JubJub     Boojum       Jabberwock
-- boowockugwup      ∷ Lens Boojum       Jabberwock Grug         Pubbawup
-- gruggazinkoom     ∷ Lens Grug         Pubbawup   Zink         Wattoom
-- zinkattumblezz    ∷ Lens Zink         Wattoom    Chumble      Spuzz
-- spuzorktrowmble   ∷ Lens Chumble      Spuzz      Gazork       Trowlg
-- gazorlglesnatchka ∷ Lens Gazork       Trowlg     Bandersnatch Yakka
-- banderyakoobog    ∷ Lens Bandersnatch Yakka      Foob         Mog

-- resultingLens ∷ Lens Snark JubJub Foob Mog
-- resultingLens = snajubjumwock . boowockugwup . gruggazinkoom . zinkattumblezz . spuzorktrowmble . gazorlglesnatchka . banderyakoobog

--------------------------------------------------------------------------------------------
--                                       Operators                                        --
--------------------------------------------------------------------------------------------

--------------------
-- Lens Operators --
--------------------

-- Action     | Operator | Type
-- -----------+----------+-----------------------------------
-- flip view  | ^.       | s → Lens' s a → a
-- set        | .∼       | Lens s t a b → b → s → t
-- over       | %∼       | Lens s t a b → (a → b) → s → t

--------------------
-- view a.k.a. ^. --
--------------------

data Payload = Payload {_weightKilos ∷ Int, _cargo ∷ String} deriving (Show)

newtype Boat = Boat {_payload ∷ Payload} deriving (Show)

makeLenses ''Payload
makeLenses ''Boat

serenity ∷ Boat
serenity = Boat (Payload 50000 "Livestock")

-- |
-- >>> view (payload . cargo) serenity
-- "Livestock"

-- |
-- ^. is the FLIPPED version of `view`.
-- >>> serenity ^. payload . cargo
-- "Livestock"

-- |
-- Looks more like Object Oriented property access.
-- >>> serenity^.payload.cargo
-- "Livestock"

-------------------
-- set a.k.a. .∼ --
-------------------

-- |
-- >>> set (payload . cargo) "Medicine" serenity
-- Boat {_payload = Payload {_weightKilos = 50000, _cargo = "Medicine"}}

-- |
-- >>> serenity & payload . cargo .~ "Medicine"
-- Boat {_payload = Payload {_weightKilos = 50000, _cargo = "Medicine"}}

------------------------------
-- Chaining Many Operations --
------------------------------

-- |
-- Using traditional actions names.
-- >>> serenity & set (payload . cargo) "Chocolate" & set (payload . weightKilos) 2310
-- Boat {_payload = Payload {_weightKilos = 2310, _cargo = "Chocolate"}}

-- |
-- Using operators.
-- >>> serenity & payload . cargo .~ "Chocolate" & payload . weightKilos .~ 2310
-- Boat {_payload = Payload {_weightKilos = 2310, _cargo = "Chocolate"}}

--------------------------
-- Using %∼ a.k.a. over --
--------------------------

-- % is often the MOD-ulo operator, and `over` MOD-ifies its focus.

-- |
-- %~ = over
-- >>> serenity & payload . weightKilos %~ subtract 1000 & payload . cargo .~ "Chocolate"
-- Boat {_payload = Payload {_weightKilos = 49000, _cargo = "Chocolate"}}

----------------------------
-- Learning Hieroglyphics --
----------------------------

-- -------------------------------------------------------------------
--                                                                  --
--  ------------------------                                        --
--  -- Legend for Getters --                                        --
--  ------------------------                                        --
--                                                                  --
--  Symbol | Description                                            --
--  -------+-------------------------------------------------       --
--    ^    | denotes a Getter                                       --
--    @    | include the index with the result                      --
--    .    | get a single value                                     --
--    ..   | get a List of values                                   --
--    ?    | maybe get the first value                              --
--    !    | force a result or throw an exception if missing        --
--                                                                  --
-- ===================================================================

-- -------------------------------------------------------------------
--                                                                  --
--  ----------------------------------                              --
--  -- Legend for Setters/Modifiers --                              --
--  ----------------------------------                              --
--                                                                  --
--  Symbol | Description                                            --
--  --------+-----------------------------------------------------  --
--    .    | set the focus                                          --
--    %    | modify the focus                                       --
--    ∼    | denotes a Setter/Modifier                              --
--    =    | denotes a Setter/Modifier over a MonadState context    --
--    <    | include the altered focus with the result              --
--    <<   | include the unaltered focus with the result            --
--    %%   | perform a traversal over the focus                     --
--    <>   | `mappend` over the focus                               --
--    ?    | wrap in Just before setting                            --
--    +    | add to the focus                                       --
--    -    | subtract from the focus                                --
--    *    | multiply the focus                                     --
--    //   | divide the focus                                       --
--    ||   | logically or the focus                                 --
--    &&   | logically and the focus                                --
--    @    | pass the index to the modification function            --
--                                                                  --
-- -------------------------------------------------------------------

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
-- `<` = get the NEW focus in addition to modification
-- >>> Thermometer 20 & temperature <+~ 15
-- (35,Thermometer {_temperature = 35})

-- |
-- Get old focus.
-- `<<` = get the OLD focus in addition to modification
-- >>> Thermometer 20 & temperature <<+~ 15
-- (20,Thermometer {_temperature = 35})

---------------------------------------------
-- When to use operators vs named actions? --
---------------------------------------------

-- Author finds it nicer to use the named versions when PARTIALLY APPLYING lens expressions, and use the operator versions the rest of the time.

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

---------------------------
-- Exercises - Operators --
---------------------------

-- 1. Consider the following list of types:

data Gate = Gate
  { _open ∷ Bool,
    _oilTemp ∷ Float
  }
  deriving (Show)

data Army = Army
  { _archers ∷ Int,
    _knights ∷ Int
  }
  deriving (Show)

data Kingdom = Kingdom
  { _kname ∷ String,
    _army ∷ Army,
    _gate ∷ Gate
  }
  deriving (Show)

makeLenses ''Gate
makeLenses ''Army
makeLenses ''Kingdom

duloc ∷ Kingdom
duloc =
  Kingdom
    { _kname = "Duloc",
      _army = Army {_archers = 22, _knights = 14},
      _gate = Gate {_open = True, _oilTemp = 10.0}
    }

-- Write a chain of expressions using infix operators to get from the start state to each of the following goal states:

-- |
-- >>> duloc & kname <>~ ": a perfect place" & army . knights *~ 3 & gate . open &&~ False
-- Kingdom {_kname = "Duloc: a perfect place", _army = Army {_archers = 22, _knights = 42}, _gate = Gate {_open = False, _oilTemp = 10.0}}

-- |
-- >>> duloc & kname <>~ "instein" & army . archers -~ 5 & army . knights +~ 12 & gate . oilTemp *~ 10
-- Kingdom {_kname = "Dulocinstein", _army = Army {_archers = 17, _knights = 26}, _gate = Gate {_open = True, _oilTemp = 100.0}}

-- |
-- >>> duloc & gate . oilTemp //~ 2 & kname <<>~ ": Home" & _2 . kname <>~ " of the talking Donkeys"
-- ("Duloc: Home",Kingdom {_kname = "Duloc: Home of the talking Donkeys", _army = Army {_archers = 22, _knights = 14}, _gate = Gate {_open = True, _oilTemp = 5.0}})

-- 2. Enter the appropriate operator in the undefined slot to make each code example consistent:

-- >>> (False, "opossums") `undefined` _1 ||~ True
-- (True, "opossums")

-- |
-- >>> (False, "opossums") & _1 ||~ True
-- (True,"opossums")

-- Remember that id is a lens which focuses the full structure.
-- >>> 2 & id `undefined` 3
-- 6

-- |
-- >>> 2 & id *~ 3
-- 6

-- >>> ((True, "Dudley"), 55.0)
--       & _1 . _2 `undefined` " - the worst"
--       & _2 `undefined` 15
--       & _2 `undefined` 2
--       & _1 . _2 `undefined` map toUpper
--       & _1 . _1 `undefined` False
-- ((False,"DUDLEY - THE WORST"),20.0)

-- |
-- >>> ((True, "Dudley"), 55.0) & _1 . _2 <>~ " - the worst" & _2 -~ 15 & _2 //~ 2 & _1 . _2 %~ map toUpper & _1 . _1 .~ False
-- ((False,"DUDLEY - THE WORST"),20.0)

-- 3. Name a lens operator that takes only two arguments.
--  view (^.)

-- 4. What's the type signature of %∼? Try to figure it without checking! Look at the examples above if you have to.
-- %∼ ∷ Lens' s a     → (a → b) → s → s
-- %∼ ∷ Lens  s t a b → (a → b) → s → t

--------------------------------------------------------------------------------------------
--                                         Folds                                          --
--------------------------------------------------------------------------------------------

---------------------------
-- Introduction to Folds --
---------------------------

-- Folds are like queries.

-- The key differences between lenses and folds are that:
-- • Lenses must focus ONE thing, Folds can focus MANY things
-- • Lenses can get and set, Folds can ONLY get.

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

-- Generic type of a fold.
-- This type says that if you give us a structure of type `s` we can find zero or more focuses of type `a`.
-- Note that a fold only specifies how to find the focuses, not how to combine them!!!
-- The decision of how to mix them all together is left up to the action.
-- Fold s a

crewMembers ∷ Fold (Set CrewMember) CrewMember
crewMembers = folded

-- `folded` takes ANY Foldable container as a structure and will focus each element inside it.
-- folded ∷ Foldable f ⇒ Fold (f a) a

----------------------------------
-- Collecting Focuses as a List --
----------------------------------

-- toListOf and its flipped operator (^..) returns a list of focuses rather than a single focus, like `view` (^.)
-- toListOf ∷ Fold s a → s → [a]
-- (^..) ∷ s → Fold s a → [a]

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

---------------------------
-- Using Lenses as Folds --
---------------------------

-- Lenses can be used directly as folds!!!
-- You can drop in a lens anywhere you need a fold.
crewRole ∷ Fold CrewMember Role
crewRole = role

-- When we use a lens as a fold we can mentally substitute the types like this:
-- `Lens' s a` becomes `Fold s a`

-- |
-- >>> let jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]
-- >>> jerry ^. role
-- PowderMonkey

---------------------
-- Composing Folds --
---------------------

-- |
-- >>> roster ^.. folded . role
-- [Gunner,PowderMonkey,Navigator,PowderMonkey]

-- |
-- crewMembers ∷ Fold (Set CrewMember) CrewMember
-- crewRole    ∷ Fold CrewMember Role
-- composes to → Fold (Set CrewMember) Role
-- Applying (^..) (`toListOf`):
-- (^..) ∷ s → Fold s a → [a]
-- Set CrewMember → Fold (Set CrewMember) Role → [Role]
-- >>> roster ^.. crewMembers . crewRole
-- [Gunner,PowderMonkey,Navigator,PowderMonkey]

-- |
-- >>> let jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]
-- >>> jerry ^.. role
-- [PowderMonkey]

-- When we use a `lens` as a `fold` we can mentally substitute the types like this:
-- `Lens' s a` becomes `Fold s a`

-----------------------------------
-- Foundational Fold Combinators --
-----------------------------------

-- `both` & `each`

-- `both` allows us to fold over BOTH parameters when the parameters are the SAME!
-- `each` allows us to fold over ALL  parameters when the parameters are the SAME!

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

------------------------------
-- Exercises - Simple Folds --
------------------------------

-- 1. What's the result of each expression? Make sure to guess before trying it out in the repl!

beastSizes ∷ [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

-- |
-- >>> beastSizes ^.. folded
-- [(3,"Sirens"),(882,"Kraken"),(92,"Ogopogo")]

-- |
-- >>> beastSizes ^.. folded . folded
-- ["Sirens","Kraken","Ogopogo"]

-- |
-- >>> beastSizes ^.. folded . folded . folded
-- "SirensKrakenOgopogo"

-- |
-- >>> beastSizes ^.. folded . _2
-- ["Sirens","Kraken","Ogopogo"]

-- |
-- >>> toListOf folded [[1, 2, 3], [4, 5, 6]]
-- [[1,2,3],[4,5,6]]

-- |
-- >>> toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]
-- [1,2,3,4,5,6]

-- |
-- >>> toListOf (folded . folded) (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
-- "CaptainFirst Mate"

-- |
-- >>> ("Hello", "It's me") ^.. both . folded
-- "HelloIt's me"

-- |
-- >>> ("Why", "So", "Serious?") ^.. each
-- ["Why","So","Serious?"]
quotes ∷ [(String, String, String)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

-- |
-- >>> quotes ^.. each
-- [("Why","So","Serious?"),("This","is","SPARTA")]

-- |
-- >>> quotes ^.. each . each
-- ["Why","So","Serious?","This","is","SPARTA"]

-- |
-- >>> quotes ^.. each . each . each
-- "WhySoSerious?ThisisSPARTA"

-- 2. Write out the specialized type for each of the requested combinators used in each of the following expressions.

-- a) folded, _1, toListOf

-- |
-- >>> toListOf (folded . _1) [(1, 'a'), (2, 'b'), (3, 'c')]
-- [1,2,3]

-- folded ∷ Fold [(Int, Char)] (Int, Char)
-- _1 ∷ Fold (Int, Char) Int
-- toListOf ∷ Fold [(Int, Char)] → [(Int, Char)] → [Int]

-- b) _2, folded, toListOf

-- |
-- >>> toListOf (_2 . folded) (False, S.fromList ["one", "two", "three"])
-- ["one","three","two"]

-- _2 ∷ Fold (Bool, Set String) (Set String)
-- folded ∷ Fold (Set String) String
-- toListOf ∷ Fold (Bool, Set String) String → (Bool, Set String) → [String]

-- c) folded, folded, toListOf

-- |
-- >>> toListOf (folded . folded) (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
-- "CaptainFirst Mate"

-- folded ∷ Fold (Map String String) String
-- folded ∷ Fold String Char
-- toListOf ∷ Fold (Map String String) Char → (Map String String) → String

-- 3. Fill in the blank with the appropriate fold to get the specified results

-- [1, 2, 3] ^.. _
-- [1, 2, 3]

-- ("Light", "Dark") ^.. _
-- ["Light"]

-- [("Light", "Dark"), ("Happy", "Sad")] ^.. _
-- ["Light","Dark","Happy","Sad"]

-- [("Light", "Dark"), ("Happy", "Sad")] ^.. _
-- ["Light","Happy"]

-- [("Light", "Dark"), ("Happy", "Sad")] ^.. _
-- "DarkSad"

-- ("Bond", "James", "Bond") ^.. _
-- ["Bond","James","Bond"]

-- Solutions:

-- |
-- >>> [1, 2, 3] ^.. folded
-- [1,2,3]

-- >>> ("Light", "Dark") ^.. _1
-- ["Light"]

-- |
-- >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . both
-- ["Light","Dark","Happy","Sad"]

-- |
-- >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1
-- ["Light","Happy"]

-- |
-- >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . folded . folded
-- "DarkSad"

-- |
-- >>> ("Bond", "James", "Bond") ^.. each
-- ["Bond","James","Bond"]

------------------
-- Custom Folds --
------------------

newtype Name = Name {getName ∷ String}
  deriving (Show)

-- Not foldable, not even having a type parameter!
data ShipCrew = ShipCrew
  { _ship ∷ Name,
    _captain ∷ Name,
    _firstMate ∷ Name,
    _conscripts ∷ [Name]
  }
  deriving (Show)

makeLenses ''ShipCrew

-- When working with lenses we had the `lens` helper which allowed us to define arbitrary lenses;
-- the equivalent helper for folds is called `folding`.
--
-- folding ∷ Foldable f ⇒ (s → f a) → Fold s a
--
-- This helper takes a projection function and returns a Fold.
-- The projection function is polymorphic over the type of Foldable it returns, but typically we just use a List.

-- projection function
collectCrewMembers ∷ ShipCrew → [Name]
collectCrewMembers crew = [_captain crew, _firstMate crew] <> _conscripts crew

-- `folding` creates a Fold.
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

------------------------
-- Mapping over Folds --
------------------------

-- ------------------------------------------------------------------------------------------------------------------------------------
-- RULE: Prefer many small and precise combinators which can be composed in different combinations to solve many different problems! --
--       Stay in the Optics world - it's very composable and resusable!
-- ------------------------------------------------------------------------------------------------------------------------------------

-- The `to` helper is pretty simple, it converts a function directly into a fold!
-- to ∷ (s → a) → Fold s a
-- It's like mapping over the Fold.

-- |
-- >>> Name "Two-faced Tony" ^. to getName
-- "Two-faced Tony"

-- |
-- We can chain many `to`s in a row
-- >>> Name "Two-faced Tony" ^. to getName . to (fmap toUpper)
-- "TWO-FACED TONY"

-- |
-- Or simply use function composition before passing to `to`.
-- However, I find it confusing to switch from reading left-to-right into right-to-left like this:
-- >>> Name "Two-faced Tony" ^. to (fmap toUpper . getName)
-- "TWO-FACED TONY"

-- |
-- Why not?
-- >>> Name "Two-faced Tony" ^. to (getName >>> fmap toUpper)
-- "TWO-FACED TONY"

-- |
-- `to` allows us to easily interleave function transformations into a path of composed optics.
-- >>> myCrew ^.. allCrewMembers . to getName
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]

----------------------------------------------------
-- Combining Multiple Folds on the Same Structure --
----------------------------------------------------

-- Every lens is a valid fold.
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

------------------------------
-- Exercises - Custom Folds --
------------------------------

-- 1. Fill in each blank with either `to`, `folded`, or `folding`.

-- >>> ["Yer", "a", "wizard", "Harry"] ^.. folded . _
-- "YerawizardHarry"

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
-- [1, 2, 4, 5]

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
-- [[1,2], [4,5]]

-- >>> ["bob", "otto", "hannah"] ^.. folded . _ reverse
-- ["bob", "otto", "hannah"]

-- >>> ("abc", "def") ^.. _ (\(a, b) → [a, b]). _ reverse . _
-- "cbafed"

-- Solutions:

-- |
-- >>> ["Yer", "a", "wizard", "Harry"] ^.. folded . folded
-- "YerawizardHarry"

-- |
-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)
-- [1,2,4,5]

-- |
-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)
-- [[1,2],[4,5]]

-- |
-- >>> ["bob", "otto", "hannah"] ^.. folded . to reverse
-- ["bob","otto","hannah"]

-- |
-- >>> ("abc", "def") ^.. folding (\(a, b) → [a, b]) . to reverse . folded
-- "cbafed"

-- 2. Fill in the blank for each of the following expressions with a path of folds which results in the specified answer.
-- Avoid partial functions and fmap.

-- The blanks were all at the end of the expression, e.g.  "[1..5] ^.. _"

-- |
-- >>> [1..5] ^.. folded . to (* 100)
-- [100,200,300,400,500]

-- |
-- >>> (1, 2) ^.. each
-- [1,2]

-- |
-- >>> [(1, "one"), (2, "two")] ^.. folded . folded
-- ["one","two"]

-- |
-- >>> (Just 1, Just 2, Just 3) ^.. each . folded
-- [1,2,3]

-- |
-- >>> [Left 1, Right 2, Left 3] ^.. folded . folded
-- [2]

-- |
-- >>> [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . both . folded
-- [1,2,3,4,5,6,7,8]

-- |
-- >>> [1, 2, 3, 4] ^.. folded . to (\n → if odd n then Left n else Right n)
-- [Left 1,Right 2,Left 3,Right 4]

-- |
-- >>> [(1, (2, 3)), (4, (5, 6))] ^.. folded . folding (\(x, (y, z)) → [x, y, z])
-- [1,2,3,4,5,6]

-- |
-- >>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\(x, y) → x ^.. folded <> y ^.. folded)
-- [1,2]

-- |
-- >>> [(1, "one"), (2, "two")] ^.. folded . folding (\(x, y) → [Left x, Right y])
-- [Left 1,Right "one",Left 2,Right "two"]

-- |
-- >>> S.fromList ["apricots", "apples"] ^.. folded . to reverse . folded
-- "selppastocirpa"

-- |
-- >>> S.fromList ["apricots", "apples"] ^.. folded . folding reverse
-- "selppastocirpa"

-- See the pattern!
-- to fn . folded == folding fn

-- 3. BONUS - Devise a fold which returns the expected results. Think outside the box a bit.

-- |
-- >>> [(12, 45, 66), (91, 123, 87)] ^.. folded . _2 . to show . to reverse . folded
-- "54321"

-- |
-- >>> [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . folding (\(x, y) → if even x then [y] else [])
-- ["b","d"]

------------------
-- Fold Actions --
------------------

--------------------------------
-- Writing Queries with Folds --
--------------------------------

-- A rule of thumb when looking for which action to use on a fold: think of the function you'd use
-- on normal ol’ Haskell list for the same purpose, then just add the suffix -Of!
-- sum → sumOf
-- minimum → minimumOf

-- sumOf ∷ Num a ⇒ Getting (Endo (Endo a)) s a → s → a

-- It's really really not important to actually know what a Getting (Endo (Endo a)) is; I don't
-- personally know the actual types of most of these actions. What I DO know though is that when
-- I see a Getting (Some Crazy Type) s a I know I can substitute nearly any optic into that slot,
-- including a (Fold s a) or a (Lens' s a). The “behind the scenes” types can unify themselves with
-- the (Some Crazy Type) portion of a Getter; so usually I mentally just substitute any Getting _ s a
-- with a Fold s a.

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
-- >>> anyOf folded (> 10) [1, 2, 3, 4]
-- False

-- |
-- Do ALL focuses match a predicate?
-- >>> allOf folded even [1, 2, 3, 4]
-- False

-- |
-- >>> allOf folded (< 10) [1, 2, 3, 4]
-- True

-- |
-- Find the first element matching a predicate
-- >>> findOf folded even [1, 2, 3, 4]
-- Just 2

-- |
-- >>> findOf folded (> 10) [1, 2, 3, 4]
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

-- Let's make this look nicer with a custom function.
-- `comparingOf` accepts a lens and will return the sort of comparator function that actions like `minimumByOf` expects.
comparingOf ∷ Ord a ⇒ Lens' s a → (s → s → Ordering)
comparingOf l = comparing (view l)

-- |
-- >>> _title <$> maximumByOf folded (comparingOf criticScore) tvShows
-- Just "How I Met Your Mother"

-- |
-- >>> minimumByOf (folded . actors . folded) (comparingOf birthYear) tvShows
-- Just (Actor {_actorName = "Anthony Head", _birthYear = 1954})

--------------------------
-- Folding with Effects --
--------------------------

-- `for_`, `traverse_`, and `mapM_` do the same thing but with different argument orderings.
-- `mapM_` has a Monad constraint instead of Applicative, an unfortunate result of the fact
-- that Applicative was discovered after Monad, so I recommend sticking with `for_` and `traverse_` !

-- traverse_ ∷ (Foldable t, Applicative f) ⇒ (a → f b) → t a → f ()
-- traverseOf_ ∷ Functor f ⇒ Fold s a → (a → f r) → s → f ()

-- for_ ∷ (Foldable t, Applicative f) ⇒ t a → (a → f b) → f ()
-- forOf_ ∷ Functor f ⇒ Fold s a → s → (a → f r) → f ()

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

-- foldOf ∷ Monoid a ⇒ Fold s a → s → a
-- foldMapOf ∷ Monoid r ⇒ Fold s a → (a → r) → s → r

-- |
-- A tuple of two Monoids is also a Monoid!
-- >>> (Sum 1, Sum 32) <> (Sum 1, Sum 20)
-- (Sum {getSum = 2},Sum {getSum = 52})

-- Transform actor into Monoid.
ageSummary ∷ Actor → (Sum Int, Sum Int)
ageSummary actor = (Sum 1, Sum (calcAge actor))

-- |
-- >>> tvShows ^.. (folded . actors . folded)
-- [Actor {_actorName = "Josh Radnor", _birthYear = 1974},Actor {_actorName = "Cobie Smulders", _birthYear = 1982},Actor {_actorName = "Neil Patrick Harris", _birthYear = 1973},Actor {_actorName = "Alyson Hannigan", _birthYear = 1974},Actor {_actorName = "Jason Segel", _birthYear = 1980},Actor {_actorName = "Sarah Michelle Gellar", _birthYear = 1977},Actor {_actorName = "Alyson Hannigan", _birthYear = 1974},Actor {_actorName = "Nicholas Brendon", _birthYear = 1971},Actor {_actorName = "David Boreanaz", _birthYear = 1969},Actor {_actorName = "Anthony Head", _birthYear = 1954}]

-- |
-- >>> tvShows ^.. (folded . actors . folded . to ageSummary)
-- [(Sum {getSum = 1},Sum {getSum = 56}),(Sum {getSum = 1},Sum {getSum = 48}),(Sum {getSum = 1},Sum {getSum = 57}),(Sum {getSum = 1},Sum {getSum = 56}),(Sum {getSum = 1},Sum {getSum = 50}),(Sum {getSum = 1},Sum {getSum = 53}),(Sum {getSum = 1},Sum {getSum = 56}),(Sum {getSum = 1},Sum {getSum = 59}),(Sum {getSum = 1},Sum {getSum = 61}),(Sum {getSum = 1},Sum {getSum = 76})]

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
-- `foldMapOf` allows to transform data into a Monoid before we fold it.
-- >>> computeAverage $ foldMapOf (folded . actors . folded) ageSummary tvShows
-- 57.2

---------------------------
-- Using ‘view’ on Folds --
---------------------------

-- A very common mistake when people get started with folds is to use `view` (^.) on a Fold instead of a Lens.
-- This is especially confusing because it actually works in some cases but not others.

-- This situation is actually caused by a bit of Lens's implementation "leaking" out.
-- The implementation of `view` accepts any type of optic, but adds constraints which usually only work on a Lens.
-- HOWEVER, if the focus you're viewing happens to be a Monoid it can "view" through the Fold by using the Monoid typeclass.

-- In general you should avoid this "weird" behaviour and just use `foldOf` explicitly when you want this behaviour!!!

-- |
-- This works just fine.
-- >>> Just "do it" ^. folded
-- "do it"

-- This one crashes and burns!
-- >>> Just (42 ∷ Int) ^. folded
-- No instance for (Monoid Int) arising from a use of ‘folded’

-- |
-- When there's a single focus, we just return it.
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

--------------------------------
-- Customizing Monoidal Folds --
--------------------------------

-- Lots of mapping folds to choose from.
-- `foldByOf`, `foldMapByOf`, `foldrOf`, `foldlOf`, ...

-- |
-- >>> foldMapOf (folded . actors . folded . actorName) (\name → M.singleton name 1) tvShows
-- fromList [("Alyson Hannigan",1),("Anthony Head",1),("Cobie Smulders",1),("David Boreanaz",1),("Jason Segel",1),("Josh Radnor",1),("Neil Patrick Harris",1),("Nicholas Brendon",1),("Sarah Michelle Gellar",1)]

-- |
-- >>> foldMapOf (folded . actors . folded . actorName) (flip M.singleton 1) tvShows
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
-- >>> foldMapByOf (folded . actors . folded . actorName) (M.unionWith (+)) mempty (flip M.singleton 1) tvShows
-- fromList [("Alyson Hannigan",2),("Anthony Head",1),("Cobie Smulders",1),("David Boreanaz",1),("Jason Segel",1),("Josh Radnor",1),("Neil Patrick Harris",1),("Nicholas Brendon",1),("Sarah Michelle Gellar",1)]

------------------------------
-- Exercises - Fold Actions --
------------------------------

-- Consider the following list of actions for the exercises below:

-- elemOf ∷ Eq a ⇒ Fold s a → a → s → Bool
-- has ∷ Fold s a → s → Bool
-- lengthOf ∷ Fold s a → s → Int
-- sumOf ∷ Num n ⇒ Fold s n → s → n
-- productOf ∷ Num n ⇒ Fold s n → s → n
-- foldOf ∷ Monoid a ⇒ Fold s a → s → a
-- preview ∷ Fold s a → s → Maybe a
-- lastOf ∷ Fold s a → s → Maybe a
-- minimumOf ∷ Ord a ⇒ Fold s a → s → Maybe a
-- maximumOf ∷ Ord a ⇒ Fold s a → s → Maybe a
-- anyOf ∷ Fold s a → (a → Bool) → s → Bool
-- allOf ∷ Fold s a → (a → Bool) → s → Bool
-- findOf ∷ Fold s a → (a → Bool) → s → Maybe a
-- foldrOf ∷ Fold s a → (a → r → r) → r → s → r
-- foldMapOf ∷ Monoid r ⇒ Fold s a → (a → r) → s → r

-- 1. Pick the matching action from the list for each example:

-- >>> _ folded []
-- False

-- >>> _ both ("Yo", "Adrian!")
-- "YoAdrian!"

-- >>> _ each "phone" ("E.T.", "phone", "home")
-- True

-- >>> _ folded [5, 7, 2, 3, 13, 17, 11]
-- Just 2

-- >>> _ folded [5, 7, 2, 3, 13, 17, 11]
-- Just 11

-- >>> _ folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
-- True

-- >>> _ folded even [11, 22, 3, 5, 6]
-- Just 22

-- Solutions:

-- |
-- >>> has folded []
-- False

-- |
-- >>> foldOf both ("Yo", "Adrian!")
-- "YoAdrian!"

-- |
-- >>> elemOf each "phone" ("E.T.", "phone", "home")
-- True

-- |
-- >>> minimumOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 2

-- |
-- >>> lastOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 11

-- |
-- >>> anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
-- True

-- |
-- >>> findOf folded even [11, 22, 3, 5, 6]
-- Just 22

-- 2. Use an action from the list along with any fold you can devise to retrieve the output from the input in each of the following challenges.

-- Is there a palindrome?
-- input = ["umbrella", "olives", "racecar", "hammer"]
-- output = Just "racecar"

-- |
-- >>> findOf folded (\x → x == reverse x) ["umbrella", "olives", "racecar", "hammer"]
-- Just "racecar"

-- Are all elements even?
-- input = (2, 4, 6)
-- output = True

-- |
-- >>> allOf each even (2, 4, 6)
-- True

-- Find pair with the largest integer.
-- input = [(2, "I'll"), (3, "Be"), (1, "Back")]
-- output = Just (3,"Be")

-- |
-- >>> maximumOf folded [(2, "I'll"), (3, "Be"), (1, "Back")]
-- Just (3,"Be")

-- Find the sum of both elements of a tuple.
-- input = (1, 2)
-- output = 3

-- |
-- >>> sumOf each (1, 2)
-- 3

-- 3. BONUS - These are a bit trickier

-- Find which word in a string has the most vowels.
-- input = "Do or do not, there is no try."
-- output = Just "there"

-- |
-- >>> maximumByOf worded (compare `on` (length . filter (`elem` "aeiou"))) "Do or do not, there is no try."
-- Just "there"

-- Combine the elements into the expected String
-- input = ["a", "b", "c"]
-- output = "cba"

-- |
-- >>> ["a", "b", "c"] ^.. to reverse . folded . folded
-- "cba"

-- |
-- >>> foldByOf folded (flip (<>)) "" ["a", "b", "c"]
-- "cba"

------------------------
-- Higher Order Folds --
------------------------

-- These sorts of combinators are higher-order, because usually they accept an optic as an argument and return a new one as a result.

----------------------
-- Taking, Dropping --
----------------------

-- There are many more signatures but here are the ones for Fold.
-- taking   ∷ Int → Fold s a → Fold s a
-- dropping ∷ Int → Fold s a → Fold s a

-- e.g.  `(taking 3)` accepts a Fold which focuses the first three focuses.

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
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded . folded
-- [1,2,3,10,20,30,100,200,300]

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. taking 2 (folded . folded)
-- [1,2]

-- |
-- >>> ("Albus", "Dumbledore") ^.. folded
-- ["Dumbledore"]

-- |
-- >>> ("Albus", "Dumbledore") ^.. both
-- ["Albus","Dumbledore"]

-- |
-- >>> ("Albus", "Dumbledore") ^.. taking 3 both
-- ["Albus","Dumbledore"]

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

-- Another higher-order Fold.
-- backwards ∷ Fold s a → Fold s a

-- |
-- >>> [1, 2, 3] ^.. backwards folded
-- [3,2,1]

-- |
-- >>> ("one", "two") ^.. backwards both
-- ["two","one"]

-- |
-- >>> [(1, 2), (3, 4)] ^.. folded
-- [(1,2),(3,4)]

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

-- Two more higher-order folds.
-- takingWhile   ∷ (a → Bool) → Fold s a → Fold s a
-- droppingWhile ∷ (a → Bool) → Fold s a → Fold s a

-- |
-- >>> [1, 5, 15, 5, 1] ^.. takingWhile (< 10) folded
-- [1,5]

-- |
-- >>> [1..100] ^.. takingWhile (< 10) folded
-- [1,2,3,4,5,6,7,8,9]

-- |
-- >>> [1..] ^.. takingWhile (< 10) folded
-- [1,2,3,4,5,6,7,8,9]

-- |
-- >>> [1..100] ^.. droppingWhile (< 90) folded
-- [90,91,92,93,94,95,96,97,98,99,100]

-- |
-- >>> [1, 5, 15, 5, 1] ^.. droppingWhile (< 10) folded
-- [15,5,1]

------------------------------------
-- Exercises - Higher Order Folds --
------------------------------------

-- 1. Fill in the blank. You'll need to remember some tricks from previous sections!

-- >>> "Here's looking at you, kid" ^.. _ 7 folded
-- "looking at you, kid"

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 _
-- ["My","Hakuna","No"]

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. _
-- ["My"]

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . _
-- "MyHakunaNo"

-- >>> import Data.Char (isAlpha)
-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . _
-- "MyHakunaNo"

-- >>> _ (10, 50, 100)
-- 60

-- >>> ("stressed", "guns", "evil") ^.. _ each
-- ["evil","guns","stressed"]

-- >>> ("stressed", "guns", "evil") ^.. backwards each . to _
-- ["live","snug","desserts"]

-- >>> import Data.Char (isAlpha)
-- >>> "blink182 k9 blazeit420" ^.. _
-- "1829420"

-- Solutions:

-- |
-- >>> "Here's looking at you, kid" ^.. dropping 7 folded
-- "looking at you, kid"

-- |
-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 worded
-- ["My","Hakuna","No"]

-- |
-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. taking 1 (folded . worded)
-- ["My"]

-- |
-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . (taking 1 worded) . folded
-- "MyHakunaNo"

-- |
-- Using `isAlpha`.
-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . takingWhile isAlpha folded
-- "MyHakunaNo"

-- |
-- >>> sumOf (taking 2 each) (10, 50, 100)
-- 60

-- |
-- >>> ("stressed", "guns", "evil") ^.. backwards each
-- ["evil","guns","stressed"]

-- |
-- >>> ("stressed", "guns", "evil") ^.. backwards each . to reverse
-- ["live","snug","desserts"]

-- |
-- >>> "blink182 k9 blazeit420" ^.. to (filter isNumber) . folded
-- "1829420"

-- |
-- >>> "blink182 k9 blazeit420" ^.. worded . droppingWhile isAlpha folded
-- "1829420"

-- 2. Solve the following problems using higher-order folds:

-- We're doing a temperature study in a given region, but we need to run tests on several subsets of the data.
-- Here's the series of daily temperature measurements we've been given for the region:

sample ∷ [Int]
sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

-- First we're interested in how many days it took until the first thaw.
-- Write an expression which calculates the number of measurements until a temp is above zero.

-- |
-- >>> lengthOf (takingWhile (< 0) folded) sample
-- 2

-- |
-- How many days in the data set?
-- >>> lengthOf folded sample
-- 10

-- We need to know the warmest it got in the first 4 days of the sample.
-- Write an expression to calculate it.

-- |
-- >>> maximumOf (taking 4 folded) sample
-- Just 4

-- Write an expression to calculate the temperature on the day AFTER we hit that temperature.
-- Use preview or ^? somewhere in that expression if you can.

-- |
-- >>> let maxTemp = maybe 1000 id (maximumOf (taking 4 folded) sample)
-- >>> sample ^? dropping 1 (droppingWhile (/= maxTemp) folded)
-- Just 3

-- How many days of below-freezing weather did we have consecutively at the END of the sample?

-- |
-- >>> length $ sample ^.. takingWhile (< 0) (backwards folded)
-- 2

-- |
-- >>> lengthOf (takingWhile (< 0) (backwards folded)) sample
-- 2

-- Now we're interested in running statistics on temperature data specifically from the first thaw until the next freeze.
-- Write an expression which lists out all temperature samples from the first time we sample above 0, until the next time we're below zero.

-- |
-- We can combine multiple modifiers to get this behaviour.
-- We drop all leading negative temperatures and then take temperatures until another freeze!
-- >>> sample ^.. takingWhile (> 0) (droppingWhile (< 0) folded)
-- [4,3,8,6]

-- BONUS: List out all the temperature samples between the FIRST thaw and the FINAL freeze.

-- |
-- >>> sample ^.. backwards (droppingWhile (< 0) (backwards (droppingWhile (< 0) folded)))
-- [4,3,8,6,-2,3]

-- Generalize this behaviour into a function: `trimmingWhile`.
-- It should drop elements from the start AND end of a fold while a predicate is True.

trimmingWhile ∷ (a → Bool) → Fold s a → Fold s a
trimmingWhile predicate = backwards . droppingWhile predicate . backwards . droppingWhile predicate

---------------------
-- Filtering Folds --
---------------------

--------------
-- Filtered --
--------------

-- Its type.
-- filtered ∷ (s → Bool) → Fold s s

-- |
-- >>> [1, 2, 3, 4] ^.. folded . filtered even
-- [2,4]

-- |
-- >>> ["apple", "passionfruit", "orange", "pomegranate"] ^.. folded . filtered ((> 6) . length)
-- ["passionfruit","pomegranate"]

-- The power comes from using `filtered` in the midst of other folds!!!

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

--- List all cards which have ANY move with an attack power greater than 40.
-- >>> deck ^.. folded . filtered (anyOf (moves . folded . movePower) (> 40)) . cardName
-- ["Elecdude","Sparkeon"]

-- |
-- How many moves do my Spark cards have in total?
-- >>> lengthOf (folded . filtered ((== Spark) . _aura) . moves . folded ) deck
-- 5

-- |
-- List all my Spark Moves with a power greater than 30.
-- >>> deck ^.. folded . filtered ((== Spark) . _aura) . moves . folded . filtered ((> 30) . _movePower) . moveName
-- ["Asplode","Shock","Battery"]

-- |
-- Using `filteredBy` we can pass a fold instead of a predicate!
-- We can continue to think in folds and keep reading left-to-right.
-- >>> deck ^.. folded . filteredBy (aura . only Spark) . moves . folded . filteredBy (movePower . filtered (> 30)) . moveName
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

---------------------------
-- Exercises - Filtering --
---------------------------

-- Use a fold to answer each of the questions about my card collection:

-- |
-- List all the cards whose name starts with 'K'.
-- >>> deck ^.. folded . filtered ((\name → head name == 'K') . _cardName)
-- [Card {_cardName = "Kapichu", _aura = Spark, _holo = False, _moves = [Move {_moveName = "Poke", _movePower = 10},Move {_moveName = "Zap", _movePower = 30}]}]

-- |
-- List all the cards whose name starts with 'K'.
-- >>> deck ^.. folded . filteredBy (cardName . filtered (\name → head name == 'K'))
-- [Card {_cardName = "Kapichu", _aura = Spark, _holo = False, _moves = [Move {_moveName = "Poke", _movePower = 10},Move {_moveName = "Zap", _movePower = 30}]}]

-- |
-- This lists the card names but not the cards itself!
-- >>> deck ^.. folded . cardName . filtered ((== 'K') . head)
-- ["Kapichu"]

-- |
-- What's the lowest attack power of all moves?
-- >>> minimumOf (folded . moves . folded . movePower) deck
-- Just 3

-- |
-- What's the name of the first card which has more than one move?
-- >>> head $ deck ^.. folded . filtered ((> 1) . length . _moves) . cardName
-- "Kapichu"

-- |
-- Are there any Hot cards with a move with more than 30 attack power?
-- >>> anyOf (folded . (filtered ((== Hot) . _aura)) . moves . folded . movePower) (> 30) deck
-- True

-- |
-- Are there any Hot cards with a move with more than 40 attack power?
-- >>> anyOf (folded . (filtered ((== Hot) . _aura)) . moves . folded . movePower) (> 40) deck
-- False

-- |
-- List the names of all holographic cards with a Wet aura.
-- >>> deck ^.. folded . filtered _holo . filteredBy (aura . only Wet) .cardName
-- ["Garydose"]

-- |
-- What's the sum of all attack power for all moves belonging to non-Leafy cards?
-- >>> sumOf (folded . filtered ((/= Leafy) . _aura) . moves . folded . movePower) deck
-- 303

---------------
-- Fold Laws --
---------------

-- There aren't any!

--------------------------------------------------------------------------------------------
--                                       Traversals                                       --
--------------------------------------------------------------------------------------------

--------------------------------
-- Introduction to Traversals --
--------------------------------

-- Traversals are essentially what you get if you combine all the powers of folds and lenses together.
-- Lenses can get and set only a single value, whereas folds can get multiple values, but can't set or update.
-- Traversals are the fusion of the two and allow us to get or set zero or more values.

-----------------------------------------------
-- How do Traversals Fit into the Hierarchy? --
-----------------------------------------------

--            |  Get   | Set/Modify | Traverse
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

-- When working with traversals rather than lenses we can no longer guarantee that our optic must have a focus.
-- Meaning we need to use the more conservative viewing actions (^?) or (^..) rather than (^.).
-- In general we need to be more careful using a traversal because it may fail.

------------------------
-- A Bit of Nostalgia --
------------------------

----------------------------
-- From Fold to Traversal --
----------------------------

-- |
-- `both` is actually a Traversal.
-- But can be used as a Fold.
-- We just view the focuses. No sets or updates.
-- >>> ("Bubbles", "Buttercup") ^.. both
-- ["Bubbles","Buttercup"]

-- |
-- Modify the focuses using the `over` action: (%∼).
-- >>> ("Bubbles", "Buttercup") & both %~ (++ "!")
-- ("Bubbles!","Buttercup!")

-- |
-- Using `set` (.∼) on a Traversal.
-- >>> ("Bubbles", "Buttercup") & both .~ "Blossom"
-- ("Blossom","Blossom")

-- |
-- Changing type of tuple (polymorphic Traversal).
-- >>> ("Bubbles", "Buttercup") & both %~ length
-- (7,9)

-- |
-- `each` is also a Traversal.
-- >>> (1, 2, 3) & each %~ (* 10)
-- (10,20,30)

-- |
-- ("Here's Johnny" ∷ Text) & each %~ toUpper
-- "HERE'S JOHNNY"

-- |
-- You can't change what Text is made of.
-- A Text block must be made up of Chars, we can't just change them to Ints and expect it to work.
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
-- Multiply all even numbers by 10.
-- >>> [1, 2, 3, 4, 5] & traversed . filtered even *~ 10
-- [1,20,3,40,5]

-- |
-- `filtered` is an extremely powerful tool, it alters the exact same elements which would be focused when you use it in a fold.
-- Reverse only the long strings.
-- >>> ("short", "really long") & both . filtered ((> 5) . length) %~ reverse
-- ("short","gnol yllaer")

---------------------------
-- Traversal Combinators --
---------------------------

--------------------------------------------
-- Traversing Each Element of a Container --
--------------------------------------------

-- |
-- Cannot modify a Fold.
-- [1, 2, 3] & folded %~ (*10)
-- Could not deduce (Contravariant Identity)
--   arising from a use of ‘folded’

-- `folded` can be used on more container types (like `Set`), but `traversed` has strictly more power (it can set and update).

-- |
-- Lists are traversable.
-- >>> [1, 2, 3] & traversed *~ 10
-- [10,20,30]

-- |
-- Tuples are traversable (over their last slot).
-- >>> ("Batman", "Superman") & traversed %~ take 3
-- ("Batman","Sup")

-- |
-- Maps are traversable.
-- >>> let powerLevels = M.fromList [("Gohan", 710) , ("Goku", 9001) , ("Krillin", 5000) , ("Piccolo", 408)]
-- >>> powerLevels & traversed %~ \n → if n > 9000 then "Over 9000" else show n
-- fromList [("Gohan","710"),("Goku","Over 9000"),("Krillin","5000"),("Piccolo","408")]

-- |
-- Sets are NOT traversable.
-- let powerLevels = S.fromList [("Gohan", 710) , ("Goku", 9001) , ("Krillin", 5000) , ("Piccolo", 408)]
-- powerLevels & traversed %~ \n → if n > 9000 then "Over 9000" else show n
-- No instance for (Traversable Set) arising from a use of ‘traversed’

-- |
-- Trees are traversable.
-- >>> let opticsTree = Node "Lens" [Node "Fold" [], Node "Traversal" []]
-- >>> opticsTree & traversed %~ reverse
-- Node {rootLabel = "sneL", subForest = [Node {rootLabel = "dloF", subForest = []},Node {rootLabel = "lasrevarT", subForest = []}]}

----------------------
-- More Combinators --
----------------------

-- |
-- As Folds `worded` works pretty much the same as `words` from the Prelude.
-- >>> "I'll be back!" ^.. worded
-- ["I'll","be","back!"]

-- |
-- The same for `lined`.
-- >>> "Run\nForrest\nRun" ^.. lined
-- ["Run","Forrest","Run"]

-- |
-- As Traversal we can also update.
-- Surround each word with '*'s.
-- Result is a String.
-- >>> "blue suede shoes" & worded %~ \s → "*" ++ s ++ "*"
-- "*blue* *suede* *shoes*"

-- |
-- Capitalize each word.
-- >>> "blue suede shoes" & worded %~ \(x:xs) → toUpper x : xs
-- "Blue Suede Shoes"

-- |
-- Add a "#" to the start of each line.
-- >>> "blue\nsuede\nshoes" & lined %~ ('#':)
-- "#blue\n#suede\n#shoes"

-- |
-- Mapping the identity function still has the white-space collapsing side-effects of `unwords`.
-- Newlines are getting lost.
-- >>> "blue \n suede \n \n shoes" & worded %~ id
-- "blue suede shoes"

---------------------------------------
-- Traversing Multiple Paths at Once --
---------------------------------------

-- Two signature examples for `beside` using tuples.
-- beside ∷ Lens s t a b → Lens s' t' a b → Traversal (s,s') (t,t') a b
-- beside ∷ Fold s a     → Fold s' a      → Fold (s,s') a

-- But will work for every Bitraversable - not only tuples - as we will see.

-- `beside` is a higher-order optic.
-- It takes two Optics as arguments and returns a new Optic as a result.

-- |
-- Note: `id` is a valid Optic.
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
-- We can modify all characters inside both halves of the tuple.
-- Each half of the tuple has a different path to focus the characters.
-- >>> ("Cowabunga", ["let's", "order", "pizza"]) & beside traversed (traversed . traversed) %~ toUpper
-- ("COWABUNGA",["LET'S","ORDER","PIZZA"])

-- |
-- Every `Bitraversable` - not only tuples - can be used with `beside`!!!
-- >>> Left (1, 2) & beside both traversed %~ negate
-- Left (-1,-2)

-- |
-- >>> Right [3, 4] & beside both traversed %~ negate ∷ Either (Int, Int) [Int]
-- Right [-3,-4]

-------------------------------------------
-- Focusing a Specific Traversal Element --
-------------------------------------------

-- `element` is simple traversal which will focuses only the nth element of a Traversable container.
-- Because it doesn't focus every element of the container it's a Monomorphic Traversal.
-- element ∷ Traversable f ⇒ Int → Traversal' (f a) a

-- |
-- >>> [0, 1, 2, 3, 4] ^? element 2
-- Just 2

-- |
-- `element` can't change the container's element type.
-- It is a `Monomorphic Traversal` after all.
-- >>> [0, 1, 2, 3, 4] & element 2 *~ 100
-- [0,1,200,3,4]

-- `elementOf` is a higher-order traversal which allows to select a specific element from an arbitrary traversal.
-- elementOf ∷ Traversal' s a → Int → Traversal' s a
-- elementOf ∷ Fold s a       → Int → Fold s a

-- |
-- `element` is basically `elementOf traversed`
-- >>> [0, 1, 2, 3, 4] ^? elementOf traversed 2
-- Just 2

-- |
-- We can get a specific element from a composition of traversals.
-- >>> [[0, 1, 2], [3, 4], [5, 6, 7, 8]] ^? elementOf (traversed . traversed) 6
-- Just 6

-- |
-- >>> [[0, 1, 2], [3, 4], [5, 6, 7, 8]] & elementOf (traversed . traversed) 6 *~ 100
-- [[0,1,2],[3,4],[5,600,7,8]]

---------------------------
-- Traversal Composition --
---------------------------

-- Each traversal selects focuses, then rebuilds the structure around the transformed results!!!

-- |
-- Capitalize the first char of every word.
-- >>> "blue suede shoes" & worded . taking 1 traversed %~ toUpper
-- "Blue Suede Shoes"

-- |
-- Find all strings longer than 5 chars then surround each word in that string with '*'.
-- >>> ["short", "really long"] & traversed . filtered ((> 5) . length) . worded %~ \s → "*" ++ s ++ "*"
-- ["short","*really* *long*"]

-- |
-- Add "Richy " to the names of people with more than $1000.
-- >>> (("Ritchie", 100000), ("Archie", 32), ("Reggie", 4350)) & each . filtered ((> 1000) . snd) . _1 %~ ("Richy " ++)
-- (("Richy Ritchie",100000),("Archie",32),("Richy Reggie",4350))

-----------------------------------
-- Exercises - Simple Traversals --
-----------------------------------

-- 1. Short answer questions:

-- What type of optic do you get when you compose a Traversal with a Fold?
-- A Fold.

-- Which of the optics we've learned can act as a Traversal?
-- Lens, Traversal.

-- Which of the optics we've learned can act as a Fold?
-- Lens, Fold, Traversal.

-- 2. Fill in the blank to complete each expression:

-- >>> ("Jurassic", "Park") & _ .~ "N/A"
-- ("N/A", "N/A")

-- >>> ("Jurassic", "Park") & both . _ .~ 'x'
-- ("xxxxxxxx", "xxxx")

-- >>> ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _ id traversed %~ take 3
-- ("Mal", ["Kay", "Ina", "Jay"])

-- >>> ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _2 . _ 1 .~ "River"
-- ("Malcolm", ["Kaylee", "River", "Jayne"])

-- This one's tricky!
-- >>> ["Die Another Day", "Live and Let Die", "You Only Live Twice"] & traversed . _ _ 1 . traversed .~ 'x'
-- [ "Die xxxxxxx Day"
-- , "Live xxx Let Die"
-- , "You xxxx Live Twice"
-- ]

-- A bit tougher now!
-- >>> ((1, 2), (3, 4)) & _ +~ 1
-- ((2, 3), (4, 5))

-- >>> (1, (2, [3, 4])) & _ +~ 1
-- (2, (3, [4, 5]))

-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & _ %~ toUpper
-- ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))

-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & _
-- ("Strawberries", "Blueberries", "Blackberries")

-- Solutions:

-- |
-- >>> ("Jurassic", "Park") & both .~ "N/A"
-- ("N/A","N/A")

-- |
-- >>> ("Jurassic", "Park") & both . each .~ 'x'
-- ("xxxxxxxx","xxxx")

-- |
-- >>> ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & beside id traversed %~ take 3
-- ("Mal",["Kay","Ina","Jay"])

-- |
-- >>> ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _2 . element 1 .~ "River"
-- ("Malcolm",["Kaylee","River","Jayne"])

-- |
-- This one's tricky!
-- >>> ["Die Another Day", "Live and Let Die", "You Only Live Twice"] & traversed . elementOf worded 1 . traversed .~ 'x'
-- ["Die xxxxxxx Day","Live xxx Let Die","You xxxx Live Twice"]

-- |
-- A bit tougher now!
-- >>> ((1, 2), (3, 4)) & both . both +~ 1
-- ((2,3),(4,5))

-- |
-- >>> (1, (2, [3, 4])) & beside id (beside id traverse) +~ 1
-- (2,(3,[4,5]))

-- |
-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each . filteredBy (_1 . only True) . _2 . taking 5 traversed %~ toUpper
-- ((True,"STRAWberries"),(False,"Blueberries"),(True,"BLACKberries"))

-- |
-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each %~ snd
-- ("Strawberries","Blueberries","Blackberries")

-----------------------
-- Traversal Actions --
-----------------------

-----------------------------
-- A Primer on Traversable --
-----------------------------

-- sequenceA ∷ (Traversable t, Applicative f) ⇒ t (f a) → f (t a)

-- `sequenceA` is flip-flopping its types:
-- [Maybe a] → Maybe [a]
-- Maybe [a] → [Maybe a]
-- Either e (IO a) → IO (Either e a)
-- [IO a] → IO [a]
-- Map k (State s a) → State s (Map k a)

-- |
-- >>> sequenceA [Just 1, Just 2, Just 3]
-- Just [1,2,3]

-- |
-- >>> sequenceA [Just 1, Nothing, Just 3]
-- Nothing

-- |
-- sequenceA ∷ Maybe (Either String a) → Either String (Maybe a)
-- >>> sequenceA $ Just (Left "Whoops")
-- Left "Whoops"

-- :t readMaybe
-- readMaybe ∷ Read a ⇒ String → Maybe a
-- 'readMaybe' is polymorphic so we need to specify a concrete result type.

-- traverse ∷ (Traversable t, Applicative f) ⇒ (a → f b) → t a → f (t b)

-- The Traversable is '[ ]', and the effect is 'IO'
-- (FilePath → IO String) → [FilePath] → IO [String]

-- The Traversable is '[ ]', and the effect is 'Maybe'
-- (String → Maybe Int) → [String] → Maybe [Int]

-- |
-- readMaybe ∷ Read a ⇒ String → Maybe a
-- >>> import Text.Read (readMaybe)
-- >>> traverse readMaybe ["1", "2", "3"] ∷ Maybe [Int]
-- Just [1,2,3]

-- |
-- >>> import Text.Read (readMaybe)
-- >>> traverse readMaybe ["1", "snark", "3"] ∷ Maybe [Int]
-- Nothing

-- The Traversable is '((,) String)', and the effect is '[ ]'
-- (Int → [Int]) → (String, Int) → [(String, Int)]

-- |
-- traverse ∷ (Integer → [Integer]) → ([Char], Integer) → [([Char], Integer)]
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

-- We want to collect all the errors.
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

-- `forOf` is the arguments-flipped version of `traverseOf`.
-- forOf ∷ Traversal s t a b → s → (a → f b) → f t

-- We can use `sequenceAOf` to pull effects deep inside our structures to the outside.
-- sequenceAOf ∷ Traversal s t (f a) a → s → f t

-- |
-- Pull out `Just`.
-- >>> sequenceAOf _1 (Just "Garfield", "Lasagna")
-- Just ("Garfield","Lasagna")

-- |
-- >>> sequenceAOf _1 (Nothing, "Lasagna")
-- Nothing

-- |
-- Pull out `Just`.
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

-----------------------------------
-- Exercises - Traversal Actions --
-----------------------------------

-- 1. Fill in the blanks! You know the drill.

-- |
--               _ _1 (Nothing, "Rosebud")
-- >>> sequenceAOf _1 (Nothing, "Rosebud")
-- Nothing

-- |
--     sequenceAOf (traversed . _1) _
-- >>> sequenceAOf (traversed . _1) [("ab", 1), ("cd", 2)]
-- [[('a',1),('c',2)],[('a',1),('d',2)],[('b',1),('c',2)],[('b',1),('d',2)]]

-- |
-- The ZipList effect groups elements by position in the list.
--     sequenceAOf _         [ZipList [1, 2], ZipList [3, 4]]
-- >>> sequenceAOf traversed [ZipList [1, 2], ZipList [3, 4]]
-- ZipList {getZipList = [[1,3],[2,4]]}

-- |
--     sequenceAOf (traversed . _2) [('a', ZipList  _),     ('b', ZipList  _)]
-- >>> sequenceAOf (traversed . _2) [('a', ZipList [1, 2]), ('b', ZipList [3, 4])]
-- ZipList {getZipList = [[('a',1),('b',3)],[('a',2),('b',4)]]}

-- |
--     let result = traverseOf _                       (\n → modify (+ n) >> get) ([1, 1, 1], (1, 1))
-- >>> let result = traverseOf (beside traversed both) (\n → modify (+ n) >> get) ([1, 1, 1], (1, 1))
-- >>> evalState result 0
-- ([1,2,3],(4,5))

-- 2. Rewrite the following using the infix-operator for traverseOf!

-- |
-- traverseOf (_1 . traversed) (\c → [toLower c, toUpper c]) ("ab", True)
-- >>> ("ab", True) & _1 . traversed %%~ (\c → [toLower c, toUpper c])
-- [("ab",True),("aB",True),("Ab",True),("AB",True)]

-- |
-- traverseOf (traversed . _1) (\c → [toLower c, toUpper c]) [('a', True), ('b', False)]
-- >>> [('a', True), ('b', False)] & traversed . _1 %%~ (\c → [toLower c, toUpper c])
-- [[('a',True),('b',False)],[('a',True),('B',False)],[('A',True),('b',False)],[('A',True),('B',False)]]

-- 3. Given the following data definitions, write a validation function which uses `traverseOf` or `%%∼`
--    to validate that the given user has an age value above zero and below 150. Return an appropriate
--    error message if it fails validation.

data AUser = AUser
  { _aname ∷ String,
    _age ∷ Int
  }
  deriving (Show)

makeLenses ''AUser

data Account = Account
  { _accId ∷ String,
    _user ∷ AUser
  }
  deriving (Show)

makeLenses ''Account

validateAge ∷ Account → Either String Account
validateAge = traverseOf (user . age) check
  where
    check accAge
      | accAge < 0 = Left "Way too young!"
      | accAge > 150 = Left "Way too old!"
      | otherwise = Right accAge

-- validateAge ∷ Account → Either String Account
-- validateAge account
--   | accAge < 0 = Left "Way too young!"
--   | accAge > 150 = Left "Way too old!"
--   | otherwise = Right account
--   where
--     accAge = account ^. user . age

-----------------------
-- Custom traversals --
-----------------------

---------------------------------
-- Optics Look like `traverse` --
---------------------------------

-- type Lens s t a b      = ∀ f. Functor f                        ⇒ (a → f b) → (s → f t)
-- type Traversal s t a b = ∀ f. Applicative f                    ⇒ (a → f b) → (s → f t)
-- type Fold s a          = ∀ f. (Contravariant f, Applicative f) ⇒ (a → f a) → (s → f s)

-- The symmetry we see in these shapes is why we can compose optics of differing types together.
-- Every optic is actually the exact same type plus or minus constraints on `f`!
-- As optics are composed, the constraints on `f` are gathered up.

--------------------------------
-- Our First Custom Traversal --
--------------------------------

-- Van Laarhoven optics are just a function which matches a ‘traverse-like’ signature.
-- We can write our own traversals by hand.

-- Simplified version of traversed which works only on lists.
-- Call the `handler` for things you want to focus on!
-- The `handler` focuses elements!
values ∷ Applicative f ⇒ (a → f b) → [a] → f [b]
values _ [] = pure []
-- values handler (a : as) = liftA2 (:) (handler a) (values handler as)
values handler (a : as) = (:) <$> handler a <*> values handler as

-- |
-- >>> ["one", "two", "three"] ^.. values
-- ["one","two","three"]

-- |
-- >>> ["one", "two", "three"] & values %~ reverse
-- ["eno","owt","eerht"]

-- |
-- type-changing transformation
-- >>> ["one", "two", "three"] & values %~ length
-- [3,3,5]

----------------------------------
-- Traversals with Custom Logic --
----------------------------------

data Transaction
  = Withdrawal {_moneyAmount ∷ Int}
  | Deposit {_moneyAmount ∷ Int}
  deriving (Show)

-- Note that it's normally bad-style to have field names on types with multiple constructors.
-- But it's okay so long as all constructors have the EXACT same field names and types like they do here.

-- This will be generated by makeLenses for `moneyAmount`.
-- For `moneyAmount` it doesn't matter if it deals with a Withdrawal or a Deposit.
-- moneyAmount ∷ Lens' Transaction Int

makeLenses ''Transaction

newtype BankAccount = BankAccount
  { _transactions ∷ [Transaction]
  }
  deriving (Show)

-- This will be generated by `makeLenses`.
-- transactions ∷ Lens' BankAccount [Transaction]

makeLenses ''BankAccount

-- |
-- Get all transactions.
-- >>> let aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]
-- >>> aliceAccount ^.. transactions . traversed
-- [Deposit {_moneyAmount = 100},Withdrawal {_moneyAmount = 20},Withdrawal {_moneyAmount = 10}]

-- |
-- Get the amounts for all transactions.
-- `moneyAmount` targets both Withdrawals and Deposits.
-- >>> let aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]
-- >>> aliceAccount ^.. transactions . traversed . moneyAmount
-- [100,20,10]

---------------------------------------
-- Case Study: Transaction Traversal --
---------------------------------------

-- deposits ∷ Traversal' [Transaction] Int ------------------------------------- This is what we want.
-- deposits ∷ Traversal [Transaction] [Transaction] Int Int -------------------- Expand to the simple Traversal.
-- deposits ∷ Applicative f ⇒ (Int → f Int) → [Transaction] → f [Transaction] -- Expand into the 'traverse'-like function.

-- The handler is for determining the focus.
-- Call the handler means focus on this value.
-- Not calling the handler means do not focus on this value.
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

-- WARNING: When focusing a subset of a list like this our first thought is often to look at using a helper like `filter t`o implement the Traversal.
-- But you need to be careful! `filter` is a destructive operation, it throws away any parts of the list which don't match.

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

-----------------------------------
-- Exercises - Custom Traversals --
-----------------------------------

-- 1. Rewrite the amount transaction lens manually as the following Traversal:

-- amountT ∷ Traversal' Transaction Int
amountT ∷ Applicative f ⇒ (Int → f Int) → Transaction → f Transaction
amountT handler (Withdrawal amt) = Withdrawal <$> handler amt
amountT handler (Deposit amt) = Deposit <$> handler amt

-- 2. Reimplement the `both traversal` over tuples:

-- both ∷ Traversal (a, a) (b, b) a b
both' ∷ Applicative f ⇒ (a → f b) → (a, a) → f (b, b)
both' handler (a1, a2) = (,) <$> handler a1 <*> handler a2

-- 3. Write the following custom Traversal:

-- transactionDelta ∷ Traversal' Transaction Int
transactionDelta ∷ Functor f ⇒ (Int → f Int) → Transaction → f Transaction
transactionDelta handler (Withdrawal amt) = Withdrawal . negate <$> handler (negate amt)
transactionDelta handler (Deposit amt) = Deposit <$> handler amt

-- It should focus the amount of the transaction, but should reflect the change that the transaction
-- causes to the balance of the account. That is, Deposits should be a positive number, but Withdrawals
-- should be negative. The Traversal should not change the underlying representation of the data.

-- Here's how it should behave:

-- |
-- >>> Deposit 10 ^? transactionDelta
-- Just 10

-- |
-- Withdrawal's delta is negative
-- >>> Withdrawal 10 ^? transactionDelta
-- Just (-10)

-- |
-- >>> Deposit 10 & transactionDelta .~ 15
-- Deposit {_moneyAmount = 15}

-- |
-- >>> Withdrawal 10 & transactionDelta .~ (-15)
-- Withdrawal {_moneyAmount = 15}

-- |
-- >>> Deposit 10 & transactionDelta +~ 5
-- Deposit {_moneyAmount = 15}

-- |
-- >>> Withdrawal 10 & transactionDelta +~ 5
-- Withdrawal {_moneyAmount = 5}

-- 4. Implement left:

-- left ∷ Traversal (Either a b) (Either a' b) a a'
left ∷ Applicative f ⇒ (a → f a') → Either a b → f (Either a' b)
left handler (Left x) = Left <$> handler x
left handler (Right x) = pure (Right x)

-- 5. BONUS: Reimplement the beside traversal:

beside' ∷ Traversal s t a b → Traversal s' t' a b → Traversal (s, s') (t, t') a b
beside' left right handler (s, s') = (,) <$> (s & left %%~ handler) <*> (s' & right %%~ handler)

-- Hint: You can use traverseOf or %%∼ to help simplify your implementation!

--------------------
-- Traversal Laws --
--------------------

-----------------------------
-- Law One: Respect Purity --
-----------------------------

-- Running the traversal with an "empty" (`pure`) handler shouldn't change anything.
-- traverseOf myTraversal pure x == pure x

-- |
-- When we run a traversal, we expect it to run our handler on the focused elements, then thread the effects through to the outside, but do nothing else!
-- >>> (traverseOf both pure ("don't", "touch") ∷ [(String, String)]) == (pure ("don't", "touch") ∷ [(String, String)])
-- True

-- |
-- >>> (traverseOf both pure ("don't", "touch") ∷ Maybe (String, String)) == (pure ("don't", "touch") ∷ Maybe (String, String))
-- True

-- This traversal is not lawful.
badTupleSnd ∷ Traversal (Int, a) (Int, b) a b
badTupleSnd handler (n, a) = (n + 1,) <$> handler a

-- The idea is that traversals shouldn't be messing around where they don't belong.
-- Let the handlers do the updating and don't mess with state yourself!

-- |
-- >>> traverseOf badTupleSnd pure (10, "Yo")
-- (11,"Yo")

-- |
-- >>> pure (10, "Yo")
-- (10,"Yo")

---------------------------------
-- Law Two: Consistent Focuses --
---------------------------------

-- x & myTraversal %~ f & myTraversal %~ g == x & myTraversal %~ (g . f)

-- The traversal should never change which elements it focuses due to alterations on those elements.

-- |
-- `both` is a law-abiding traversal, so we expect to see the equality hold for any structure or handlers we can dream up.
-- >>> ((0, 0) & both %~ (+ 10) & both %~ (* 10)) == ((0, 0) & both %~ (* 10) . (+ 10))
-- True

-- |
-- `filtered` is a law-breaking traversal. Updates can change which elements are in focus!
-- >>> (2 & filtered even %~ (+ 1) & filtered even %~ (* 10)) == (2 & filtered even %~ (* 10) . (+ 1))
-- False

-- |
-- >>> 2 & filtered even %~ (+ 1) & filtered even %~ (* 10)
-- 3

-- |
-- `filtered` is a law-breaking traversal.
-- >>> 2 & filtered even %~ (* 10) . (+ 1)
-- 30

----------------------------------
-- Good Traversal Bad Traversal --
----------------------------------

-- Why is there a traversal included in the lens library that blatantly breaks one of the laws?
-- The traversal laws are more guidelines than anything.

-- Some traversals like `filtered` are very useful but can't be encoded in a legal way in Haskell.

--------------------------------
-- Exercises - Traversal Laws --
--------------------------------

-- 1. `worded` is a law-breaking traversal! Determine which law it breaks and give an example which shows that it doesn't pass the law.

-- Passes 1st law.

-- |
-- >>> traverseOf worded pure "Hello again my friend"
-- "Hello again my friend"

-- |
-- >>> pure "Hello again my friend"
-- "Hello again my friend"

-- |
-- Adding a space to the string during an update creates new focuses for the worded traversal!
-- >>> ("one two" & worded %~ (<> " missisipi") & worded %~ reverse) == ("one two" & worded %~ reverse . (<> " missisipi"))
-- False

-- 2. Write a custom traversal which breaks the first law. Be as creative as you like!
-- NA

-- 3. Write a custom traversal which breaks the second law. Be as creative as you like!
-- NA

-- 4. For each of the following traversals, decide which you think is lawful.
--    If they're unlawful come up with a counter-example for one of the laws.

-- taking: ✓
-- beside: ✓
-- each: ✓
-- lined: ✗ (Adding a newline creates new focuses for the next lined traversal.)
-- traversed: ✓

---------------------------
-- Advanced Manipulation --
---------------------------

-- partsOf ∷ Traversal' s a → Lens' s [a]

-- The lens generated by partsOf takes all the focuses of the provided traversal and packs them into a
-- list to be manipulated. Then, it takes the modified list and maps each element back into the original structure!

-- Rules regarding replacement:
-- If the list has more elements than the traversal, the extras will be ignored.
-- If the list has fewer elements than the traversal, unmatched portions of the traversal will be unaffected.

-- It's because of these rules that `partsOf` isn't a polymorphic lens.
-- We might need to re-use some of the original elements so we can't change their type!

-- Get

-- |
-- >>> [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _1)
-- "abc"

-- |
-- >>> [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _2)
-- [1,2,3]

-- set

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

-- Modify (over)

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

-- We can change the type of the focus but we if set or modify with the wrong number of list elements it'll crash.
-- unsafePartsOf ∷ Traversal s t a b → Lens s t [a] [b]

-- |
-- [('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) .~ [True, False]
-- unsafePartsOf': not enough elements were supplied

-- |
-- >>> [('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) .~ [True, False, True]
-- [(True,1),(False,2),(True,3)]

-- |
-- >>> [('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) %~ \xs → zipWith (,) xs ((Just <$> tail xs) ++ [Nothing])
-- [(('a',Just 'b'),1),(('b',Just 'c'),2),(('c',Nothing),3)]

-------------------------
-- Exercises - partsOf --
-------------------------

-- What fits in the blanks? (Sometimes blanks are missing. Just ignore. Exercise is a bit chaotic.)

-- |
-- Viewing
-- >>> [1, 2, 3, 4] ^. partsOf (traversed . filtered even)
-- [2,4]

-- |
-- >>> ["Aardvark", "Bandicoot", "Capybara"] ^. traversed . partsOf (taking 3 traversed)
-- "AarBanCap"

-- |
--     ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^. partsOf _
-- >>> ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^. partsOf (beside traversed each)
-- [1,2,3,4]

-- Setting

-- |
--     [1, 2, 3, 4] & partsOf (traversed . _            ) .~ [20, 40]
-- >>> [1, 2, 3, 4] & partsOf (traversed . filtered even) .~ [20, 40]
-- [1,20,3,40]

-- |
--     ["Aardvark", "Bandicoot", "Capybara"] & partsOf _                       .~ "Kangaroo"
-- >>> ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Kangaroo"
-- ["Kangaroo","Bandicoot","Capybara"]

-- |
-- >>> ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Ant"
-- ["Antdvark","Bandicoot","Capybara"]

-- Modifying

-- |
-- Map values are traversed in order by key!
-- >>> M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')] & partsOf traversed %~ \(x:xs) → xs ++ [x]
-- fromList [('a','b'),('b','c'),('c','a')]

-- |
-- >>> ('a', 'b', 'c') & partsOf each %~ reverse
-- ('c','b','a')

-- Bonus

-- |
-- >>> [1, 2, 3, 4, 5, 6] & partsOf (taking 3 traversed) %~ reverse
-- [3,2,1,4,5,6]

-- |
-- >>> ('a', 'b', 'c') & unsafePartsOf each %~ \xs → fmap ((,) xs) xs
-- (("abc",'a'),("abc",'b'),("abc",'c'))

--------------------------------------------------------------------------------------------
--                               Indexable Structures                                     --
--------------------------------------------------------------------------------------------

------------------------------------
-- What's an Indexable Structure? --
------------------------------------

-- Lists allow getting values by an Int index.

-- |
-- Getting values at an index using `!!`.
-- >>> ["Henry I", "Henry II", "Henry III"] !! 0
-- "Henry I"

-- |
-- >>> ["Henry I", "Henry II", "Henry III"] !! 1
-- "Henry II"

-- |
-- ["Henry I", "Henry II", "Henry III"] !! 3
-- Prelude.!!: index too large

-- Maps use `lookup` to get values at an index.

-- |
-- >>> M.lookup "Leo" (M.fromList [("Leo", "Katanas"), ("Raph", "Sai")])
-- Just "Katanas"

-- |
-- Maps use `adjust` to update the value at an index
-- >>> M.adjust ("Two " <>) "Leo" (M.fromList [("Leo", "Katanas"), ("Raph", "Sai")])
-- fromList [("Leo","Two Katanas"),("Raph","Sai")]

-----------------------------------------------
-- Accessing and Updating Values with 'Ixed' --
-----------------------------------------------

--------------------
-- The Ixed Class --
--------------------

-- class Ixed m where
--   ix ∷ Index m → Traversal' m (IxValue m)

-- Index and IxValue are type families.
-- They tell us which index type or value type to use for a given structure.

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

-- `ix` can neither create nor delete slots in an indexed structure, doing so would actually break the second Traversal law!
-- Don't worry though, we'll learn a perfectly law abiding way to insert into Maps soon.

-- |
-- Get the value at index 1:
-- >>> ["Borg", "Cardassian", "Talaxian"] ^? ix 1
-- Just "Cardassian"

-- |
-- There's no value at index 10 so the traversal doesn't focus anything.
-- >>> ["Borg", "Cardassian", "Talaxian"] ^? ix 10
-- Nothing

-- |
-- It's a traversal, so we can `set` new values at that index.
-- >>> ["Borg", "Cardassian", "Talaxian"] & ix 1 .~ "Vulcan"
-- ["Borg","Vulcan","Talaxian"]

-- |
-- A `set` will do nothing if the given index doesn't have a value.
-- >>> ["Borg", "Cardassian", "Talaxian"] & ix 10 .~ "Romulan"
-- ["Borg","Cardassian","Talaxian"]

-- |
-- Get the value at key "Zuko".
-- >>> M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] ^? ix "Zuko"
-- Just "Fire"

-- |
-- If there's no value at a key, the traversal returns zero elements.
-- >>> M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] ^? ix "Sokka"
-- Nothing

-- |
-- We can set the value at a key, but only if that key already exists.
-- >>> M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & ix "Toph" .~ "Metal"
-- fromList [("Katara","Water"),("Toph","Metal"),("Zuko","Fire")]

-- |
-- Setting a non-existent element of a Map does NOT insert it.
-- >>> M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & ix "Iroh" .~ "Lightning"
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
-- The empty list represents the value at the root.
-- 0 goes left, 1 goes right, empty goes nowhere.
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

-- The At typeclass allows focusing values within map-like structures which allow arbitrary insertion or deletion.
-- This is not possible with lists. You can't insert a 10th element unless you have a 9th!

-- class At where
--   at ∷ Index m → Lens' m (Maybe (IxValue m))

-- For comparison, here's `ix` and `at` side-by-side:
-- ix ∷ Index m → Traversal' m (IxValue m)
-- at ∷ Index m → Lens'      m (Maybe (IxValue m))

-- |
-- >>> M.insert "Mikey" "Nunchaku" $ M.fromList [("Leo", "Katanas"), ("Raph", "Sai")]
-- fromList [("Leo","Katanas"),("Mikey","Nunchaku"),("Raph","Sai")]

-- |
-- >>> M.delete "Leo" $ M.fromList [("Leo", "Katanas"), ("Raph", "Sai")]
-- fromList [("Raph","Sai")]

-- To insert or replace an element we can set a value wrapped in Just; to delete we can set the focus to Nothing.
-- We can update a value arbitrarily using `over` and providing a function from `Maybe a → Maybe a`.

-- |
-- Since 'at' creates a lens, we use `^.` instead of `^?`
-- >>> M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] ^. at "Zuko"
-- Just "Fire"

-- |
-- >>> M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & at "Zuko" .~ Nothing
-- fromList [("Katara","Water"),("Toph","Earth")]

-- |
-- >>> M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & at "Iroh" .~ Just "Lightning"
-- fromList [("Iroh","Lightning"),("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]

-- |
-- Using the `?~` operator we can avoid writing out the Just when inserting elements.
-- >>> M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & at "Iroh" ?~ "Lightning"
-- fromList [("Iroh","Lightning"),("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]

-- |
-- We can use this with other optics if we want, but it's usually used with `at`.
-- >>> (1, 2) & both ?~ "twins!"
-- (Just "twins!",Just "twins!")

-- |
-- `sans` is just short-hand for setting the value at an index to `Nothing`.
-- >>> sans "Katara" $ M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- fromList [("Toph","Earth"),("Zuko","Fire")]

-----------------------
-- Manipulating Sets --
-----------------------

-- One way to imagine a Set is as a map where the set elements are keys: (Map v ()).
-- Using unit `()` as the value means the only real information stored in the Map is whether a value exists or not.
-- Just () means it exists, Nothing means it does not exist.

primes ∷ Set Int
primes = S.fromList [2, 3, 5, 7, 11, 13]

-- |
-- >>> primes ^? ix 5
-- Just ()

-- |
-- >>> primes ^? ix 4
-- Nothing

-- |
-- Insert 17 into the Set.
-- >>> primes & at 17 ?~ ()
-- fromList [2,3,5,7,11,13,17]

-- |
-- Remove 4 from the Set.
-- >>> sans 5 primes
-- fromList [2,3,7,11,13]

-- |
-- We can use `&` to chain uses of `sans`.
-- >>> primes & sans 5 & sans 7 & sans 11
-- fromList [2,3,13]

--------------------------------------
-- Exercises - Indexable Structures --
--------------------------------------

-- 1. Fill in the blanks!

-- |
--     ["Larry", "Curly", "Moe"] & _  1 .~ "Wiggly"
-- >>> ["Larry", "Curly", "Moe"] & ix 1 .~ "Wiggly"
-- ["Larry","Wiggly","Moe"]

-- |
--     M.fromList [("Superman", "Lex"), ("Batman", "Joker")] & _  "Spiderman" .~ Just "Goblin"
-- >>> M.fromList [("Superman", "Lex"), ("Batman", "Joker")] & at "Spiderman" .~ Just "Goblin"
-- fromList [("Batman","Joker"),("Spiderman","Goblin"),("Superman","Lex")]

-- |
--        _ "Superman" $ M.fromList [("Superman", "Lex"), ("Batman", "Joker")]
-- >>> sans "Superman" $ M.fromList [("Superman", "Lex"), ("Batman", "Joker")]
-- fromList [("Batman","Joker")]

-- |
--     S.fromList ['a', 'e', 'i', 'o', 'u'] & at 'y' _  () & at 'i' .~ _
-- >>> S.fromList ['a', 'e', 'i', 'o', 'u'] & at 'y' ?~ () & at 'i' .~ Nothing
-- fromList "aeouy"

-- 2. Use `ix` and `at` to go from `input` to `output`!

-- input  = M.fromList [("candy bars",13),("soda",34),("gum",7)]
-- output = M.fromList [("candy bars",13),("ice cream",5),("soda",37)]

-- |
-- >>> let sweets = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]
-- >>> M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)] & sans "gum" & ix "soda" +~ 3 & at "ice cream" ?~ 5
-- fromList [("candy bars",13),("ice cream",5),("soda",37)]

------------------------------------
-- Custom Indexed Data Structures --
------------------------------------

------------------------------------
-- Custom Ixed: Cyclical Indexing --
------------------------------------

newtype Cycled a = Cycled [a]
  deriving (Show)

type instance Index (Cycled a) = Int

type instance IxValue (Cycled a) = a

instance Ixed (Cycled a) where
  -- ix ∷ Index m → Traversal' m (IxValue m)
  -- ix ∷ Index (Cycled a) → Traversal' (Cycled a) (IxValue (Cycled a))
  -- ix ∷ Int              → Traversal' (Cycled a)  a
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
  -- at ∷ Index m → Lens' m (Maybe (IxValue m))
  -- at ∷ Index PostalAddress → Lens' PostalAddress (Maybe (IxValue PostalAddress))
  -- at ∷ AddressPiece        → Lens' PostalAddress (Maybe  String)
  at ∷ AddressPiece → Lens' PostalAddress (Maybe String)
  at BuildingNumber = buildingNumber
  at StreetName = streetsName
  at ApartmentNumber = apartmentNumber
  at PostalCode = postalCode

-- |
-- >>> PostalAddress Nothing Nothing Nothing Nothing
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Nothing, _apartmentNumber = Nothing, _postalCode = Nothing}

-- |
-- >>> PostalAddress Nothing Nothing Nothing Nothing & at StreetName ?~ "Baker St." & at ApartmentNumber ?~ "221B"
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Just "Baker St.", _apartmentNumber = Just "221B", _postalCode = Nothing}

-- |
-- >>> PostalAddress Nothing Nothing Nothing Nothing & at StreetName ?~ "Baker St." & at ApartmentNumber ?~ "221B" & ix ApartmentNumber .~ "221A"
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Just "Baker St.", _apartmentNumber = Just "221A", _postalCode = Nothing}

-- |
-- >>> PostalAddress Nothing Nothing Nothing Nothing & at StreetName ?~ "Baker St." & at ApartmentNumber ?~ "221B" & sans StreetName
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Nothing, _apartmentNumber = Just "221B", _postalCode = Nothing}

-------------------------------------------
-- Exercises – Custom Indexed Structures --
-------------------------------------------

-- TODO
-- 1. Implement both Ixed and At for a newtype wrapper around a Map which makes indexing case insensitive, you can specialize to String or Text keys.
--    Write the ix instance manually even though it has a default implementation.
--    It's okay to assume that user will only interact with the map via Ixed and At.

-----------------------------
-- Handling Missing Values --
-----------------------------

--------------------------------------
-- Checking whether Updates Succeed --
--------------------------------------

-- It takes an update function and will apply it to all focuses.
-- If it doesn't find any focuses at all then it will return the
-- value of empty for whichever Alternative type is inferred as
-- the return value, but we usually use Nothing from Maybe.

-- actual type signature
-- failover ∷ Alternative m ⇒ LensLike ((,) Any) s t a b → (a → b) → s → m t

-- specialized type signature
-- failover ∷ Traversal s t a b → (a → b) → s → Maybe t

-- |
-- There's no element at index 6, so the update fails
-- >>> "abcd" & failover (ix 6) toUpper ∷ Maybe String
-- Nothing

-- |
-- There's an element at index 2, so the update succeeds!
-- >>> "abcd" & failover (ix 2) toUpper ∷ Maybe String
-- Just "abCd"

-- |
-- >>> [] & failover _head (* 10) ∷ Maybe [Int]
-- Nothing

-- |
-- We can nest traversals; the whole chain fails if it focuses no elements.
-- >>> M.fromList[('a', [1, 2])] & failover (ix 'a' . ix 3) (* 10) ∷ Maybe (M.Map Char [Int])
-- Nothing

-- |
-- >>> M.fromList[('a', [1, 2])] & failover (ix 'a' . ix 1) (* 10) ∷ Maybe (M.Map Char [Int])
-- Just (fromList [('a',[1,20])])

-- |
-- It even works with filters.
-- >>> [1, 3, 5] & failover (traversed . filtered even) (* 10) ∷ Maybe [Int]
-- Nothing

-- |
-- >>> [1, 3, 5] & failover (traversed . filtered odd) (* 10) ∷ Maybe [Int]
-- Just [10,30,50]

-- |
-- First update will succeed. Note the Alternative constraint on `failover`.
-- >>> let s = "abcdefg"
-- >>> failover (ix 8) toUpper s <|> failover (ix 6) toUpper s <|> failover (ix 4) toUpper s
-- "abcdefG"

------------------------------
-- Fallbacks with `failing` --
------------------------------

-- `failing` combines two optics by trying the first, and falling back on the second.

-- specialized signatures
-- failing ∷ Fold      s t a b → Fold      s t a b → Fold      s t a b
-- failing ∷ Traversal s t a b → Traversal s t a b → Traversal s t a b

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

----------------------------------
-- Default Elements Using `non` --
----------------------------------

-- We configure `non` by passing it a default value.
-- It uses the default value to build a traversal which focuses the value within a Just,
-- or in the case of a Nothing focuses the default value instead.

-- For all intents and purposes we'll pretend `non` has this signature:
-- non ∷ Eq a ⇒ a → Traversal' (Maybe a) a

-- This mimics the behavior of `fromMaybe` using optics --

-- |
-- >>> Nothing ^. non 0
-- 0

-- |
-- >>> Nothing ^. non "default"
-- "default"

-- |
-- >>> Just "value" ^. non "default"
-- "value"

-- `non` becomes much more useful when combined with the `at` combinator --

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

-- `non` has interesting behavior when setting a key to the default value.
-- It maps the default value back to Nothing, which `at` will be exclude from the map.
-- If something is the default value it's redundant to store it.
-- If we try to view that key later we'll still receive the correct value by using the default.
-- We can use this to build sparse maps and save some performance if one particular value in the map is very common.

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

-- Let's keep tally of the number of hours each employee has worked in a Map.
-- If we add hours for an employee missing from the map they should be added as though they had logged 0 hours.

-- |
-- Erin will be added to the map since she's missing.
-- >>> M.fromList [("Jim", 32), ("Dwight", 39)] & at "Erin" . non 0 +~ 10
-- fromList [("Dwight",39),("Erin",10),("Jim",32)]

-- |
-- Since Jim already has hours logged we simply increment them.
-- >>> M.fromList [("Jim", 32), ("Dwight", 39)] & at "Jim" . non 0 +~ 8
-- fromList [("Dwight",39),("Jim",40)]

-- |
-- When we pay-out an employee's hours, we set their hours to `0`
-- `non` removes any keys with the default value from the list entirely!
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

-- `pre` is a version of `preview` as a higher-order optic.
-- You pass it a fold and it will try to get a value from it, returning `Nothing` if it can't find one.
-- Note that it returns a Getter, so we can't set or update using `pre`.

-- specialized signature
-- pre ∷ Fold s a → Getter s (Maybe a)

-- |
-- We can combine this with `non` and `ix` to get default values when accessing list elements.
-- >>> "abc" ^. pre (ix 10) . non 'z'
-- 'z'

-- |
-- We use ^. rather than ^? since `pre` turns the fold into a Getter.
-- >>> [1, 2, 3, 4] ^. pre (traversed . filtered even)
-- Just 2

-- |
-- >>> [1, 3] ^. pre (traversed . filtered even)
-- Nothing

--------------------------------
-- Exercises - Missing Values --
--------------------------------

-- 1. Write an optic which focuses the value at key "first" or, failing that, the value at key "second".

-- |
-- >>> let optic = ix "first" `failing` ix "second"
-- >>> M.fromList [("first", False), ("second", False)] & optic .~ True
-- fromList [("first",True),("second",False)]

-- |
-- >>> let optic = ix "first" `failing` ix "second"
-- >>> M.fromList [("second", False)] & optic .~ True
-- fromList [("second",True)]

-- 2. Write an optic which focuses the first element of a tuple iff it is even, and the second tuple element otherwise.
--    Assume each slot contains an integer.

-- |
-- >>> let optic = ix 0 . filtered even `failing` ix 1
-- >>> (2, 2) & optic *~ 10
-- (20,2)

-- |
-- >>> let optic = ix 0 . filtered even `failing` ix 1
-- >>> (1, 1) & optic *~ 10
-- (1,10)

-- 3. Write an optic which focuses all even numbers in a list, if none of the members are even then focus ALL numbers in the list.

-- |
-- >>> let optic = traversed . filtered even `failing` traversed
-- >>> [1, 2, 3, 4] ^.. optic
-- [2,4]

-- |
-- >>> let optic = traversed . filtered even `failing` traversed
-- >>> [1, 3, 5] ^.. optic
-- [1,3,5]

-- 4. Fill in the blanks!

-- |
-- >>> Nothing ^. non "default"
-- "default"

-- |
--     Nothing & _       +~ 7
-- >>> Nothing & non 100 +~ 7
-- Just 107

-- |
--     M.fromList [("Perogies", True), ("Pizza", True), ("Pilsners", True)] ^. at "Broccoli" . _
-- >>> M.fromList [("Perogies", True), ("Pizza", True), ("Pilsners", True)] ^. at "Broccoli" . non False
-- False

-- |
--     M.fromList [("Breath of the wild", 22000000), ("Odyssey", 9070000)] & _                          +~ 999
-- >>> M.fromList [("Breath of the wild", 22000000), ("Odyssey", 9070000)] & at "Wario's Woods" . non 0 +~ 999
-- fromList [("Breath of the wild",22000000),("Odyssey",9070000),("Wario's Woods",999)]

-- |
--     ["Math", "Science", "Geography"] ^. _            . non "Unscheduled"
-- >>> ["Math", "Science", "Geography"] ^. pre (ix 100) . non "Unscheduled"
-- "Unscheduled"

-- BONUS: Excellent example for `pre`!
-- Use 'pre' and 'non'

-- |
--     [1, 2, 3, 4] ^.. _
-- >>> [1, 2, 3, 4] ^.. traversed . pre (filtered even) . non (-1)
-- [-1,2,-1,4]

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

-- Prisms can be run BACKWARDS, taking a focus and embedding it into a structure!
-- We'll see how prisms are a natural candidate for specifying PATTERN-MATCHING semantics and help to work with sum-types as well.

-- For running a Prism forwards  run `preview` (^?).
-- For running a Prism backwards run `review`  (#).
--

------------------------------------
-- Simple Pattern-Matching Prisms --
------------------------------------

-- For the `Either` type we have `_Left` and `_Right` prisms.
-- _Left  ∷ Prism (Either l r) (Either l' r ) l l'
-- _Right ∷ Prism (Either l r) (Either l  r') r r'

-- These prisms will pattern match on the Left or Right side of the either type, focusing on the value inside.
-- We can use `preview` (^?) to "run" the pattern match, returning Nothing if it's not a match:

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
-- Since prisms are valid traversals we can set, update, or traverse the focused value through them.
-- >>> Left 10 & _Left +~ 5
-- Left 15

-- |
-- >>> Right "howdy" & _Right %~ reverse
-- Right "ydwoh"

-- Analogously for `Maybe`.
-- _Nothing ∷ Prism' (Maybe a) ()
-- _Just    ∷ Prism  (Maybe a) (Maybe b) a b

-- |
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
-- >>> Just 20 & _Just %~ (+ 10)
-- Just 30

-----------------------------------------------------------
-- Checking Pattern Matches with Prisms (`has`, `isn't`) --
-----------------------------------------------------------

-- has   ∷ Fold  s   a   → s → Bool
-- isn't ∷ Prism s t a b → s → Bool

-- `has` checks whether the given fold yields any elements when run on the provided structure; while
-- `isn't` returns whether a given prism does not match the input.

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

-- A path is a list of URL segments.
type Path = [String]

type Body = String

data Request
  = Post Path Body
  | Get Path
  | Delete Path
  deriving (Show)

-- Creates `_Post` `_Get` and `_Delete` prisms.
makePrisms ''Request

-- See also `:browse` in the REPL:
-- _Post   ∷ Prism' Request (Path, Body)  -- Note the tuple!
-- _Get    ∷ Prism' Request  Path
-- _Delete ∷ Prism' Request  Path

-- This is all we need to use these prisms for getting or setting as if they were traversals.

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

-- Every prism represents a pattern-match which can be REVERSED!
-- By feeding a prism a focus we can embed that focus into a structure via the context implied by the pattern!
-- Even though viewing through a prism may fail if the pattern doesn't match, embedding a value into a pattern using a prism always succeeds!
-- To run a prism backwards we use the `review` (#), which you can think of as short for "reverse view".
-- It embeds the focus into the prism's pattern.

-- (#) is comparable to the pipe operator in PureScript.
-- PureScript's (#) is (&) in Haskell.

-- review ∷ Prism s t a b → b → t
-- (#)    ∷ Prism s t a b → b → t

-- Prisms which match on a constructor of some type can be reversed to embed fields into the constructor.
-- The reverse of unpacking a specific constructor is to pack those fields into that constructor.

-- |
-- >>> Get ["posts"]
-- Get ["posts"]

-- |
-- >>> review _Get ["posts"]
-- Get ["posts"]

-- |
-- using the infix operator
-- >>> _Get # ["posts"]
-- Get ["posts"]

-- |
-- >>> Delete ["posts"]
-- Delete ["posts"]

-- |
-- >>> _Delete # ["posts"]
-- Delete ["posts"]

-- |
-- >>> Post ["posts"] "My blog post"
-- Post ["posts"] "My blog post"

-- |
-- Constructors with multiple fields accept a tuple of the fields.
-- >>> _Post # (["posts"], "My blog post")
-- Post ["posts"] "My blog post"

-- |
-- >>> Left "an error"
-- Left "an error"

-- |
-- constructing a `Left` from a string
-- >>> review _Left "an error"
-- Left "an error"

-- |
-- constructing a `Left` from a string
-- >>> _Left # "an error"
-- Left "an error"

-- |
-- >>> Right 42
-- Right 42

-- |
-- >>> review _Right 42
-- Right 42

-- |
-- >>> _Right # 42
-- Right 42

-- |
-- Since composing prisms results in a new prism, we can compose prisms before passing them to review to build a nested constructor function.
-- >>> _Just . _Left # 1337
-- Just (Left 1337)

-- We could and should use the normal constructors.
-- But, the ability to reverse a prism can be used to implement some of the prism combinators we'll look at later on.

-----------------------------
-- Other Types of Patterns --
-----------------------------

-- Although most of the prisms you'll encounter will be used for matching on data type constructors, prisms can also encode more complex and abstract patterns.
-- Unlike regular pattern matching if a prism fails to match it won't crash, instead the prism simply won't focus anything.

-- The `_Cons` prism handles the common task of peeling an element off the top of a list-like type, e.g. lists, vectors, Strings, ...

-- |
-- `_Cons` is a prism.
-- >>> [1, 2, 3] ^? _Cons
-- Just (1,[2,3])

-- specialized type signature for String
-- _Cons ∷ Prism' String (Char, String)

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
-- Since `_Cons` is a prism we can run it backwards using `review` (#) to cons an element onto the front of a list-like type.
-- This operation will never fail!
-- >>> _Cons # ('F', "reedom")
-- "Freedom"

-- |
-- >>> "Freedom" & _tail %~ reverse
-- "Fmodeer"

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
-- The phrasing for 'has' isn't quite as clear.
-- Feel free to simply define 'is = has' if that helps!!!
-- >>> has _Empty M.empty
-- True

-- |
-- >>> has _Empty (S.fromList [1, 2, 3])
-- False

-- The `_Show` prism can Read or Show values to and from their String representations.
-- The "pattern" we're matching on is whether the value can successfully be parsed into the result type (which will be determined by type inference).
-- If the string fails to parse into the output type properly the prism will not match.
-- To run it in reverse it calls "show" on the provided value to turn it back into a string.

-- _Show ∷ (Read a, Show a) ⇒ Prism' String a

-- |
-- >>> "12" ^? _Show ∷ Maybe Int
-- Just 12

-- |
-- The type we assert is important. If we pick a different type it changes the behavior.
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

------------------------
-- Exercises - Prisms --
------------------------

-- 1. Which prisms will be generated from the following data declaration? Give their names and types!

data ContactInfo
  = CiEmail String
  | CiTelephone Int
  | CiAddress String String String

makePrisms ''ContactInfo

-- _CiEmail     ∷ Prism' ContactInfo String
-- _CiTelephone ∷ Prism' ContactInfo Int
-- _CiAddress   ∷ Prism' ContactInfo (String, String, String)

-- 2. Fill in the blanks

-- |
--     Right 35 & _ +~ 5
-- >>> Right 35 & _Right +~ 5
-- Right 40

-- |
--     [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] ^.. folded . _
-- >>> [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] ^.. folded . _Just
-- ["Mind","Power","Soul","Time"]

-- |
--     [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] & _ <>~ " Stone"
-- >>> [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] & traversed . _Just <>~ " Stone"
-- [Just "Mind Stone",Just "Power Stone",Nothing,Just "Soul Stone",Nothing,Just "Time Stone"]

-- |
--     Left (Right True, "Eureka!") & _ %~ not
-- >>> Left (Right True, "Eureka!") & _Left . _1 . _Right %~ not
-- Left (Right False,"Eureka!")

-- |
--     _Cons _ ("Do",["Re", "Mi"])
-- >>> _Cons # ("Do",["Re", "Mi"])
-- ["Do","Re","Mi"]

-- |
--     isn't (_Show ∷ _) "not an int"
-- >>> isn't (_Show ∷ Prism' String Bool) "not an int"
-- True

-- 3. Write expressions to get the output from the provided input!

-- a)
-- input  = (Just 1, Nothing, Just 3)
-- output = [1, 3]

-- |
-- >>> (Just 1, Nothing, Just 3) ^.. each . _Just
-- [1,3]

-- b)
-- input  = ('x', "yz")
-- output = "xzy"

-- |
-- >>> ('x', "yz") & review _Cons & _tail %~ reverse
-- "xzy"

-- c)
-- input  = "do the hokey pokey"
-- output = Left (Just (Right "do the hokey pokey"))

-- |
-- >>> _Left . _Just . _Right # "do the hokey pokey"
-- Left (Just (Right "do the hokey pokey"))

---------------------------
-- Writing Custom Prisms --
---------------------------

-- We can build a prism for any pattern which we can reverse.
-- We can imagine a prism as "refracting" your data, splitting it up and focusing only the pieces which match the pattern, dissipating the rest.
-- Luckily the lens library provides us with some helpers to make constructing prisms easy and fool-proof.

--         EMBEDDING FUNCTION
--             |
--             |     MATCHING FUNCITON
--             |             |
-- prism  ∷ (b → t) → (s → Either t a) → Prism s t a b
-- prism' ∷ (b → s) → (s → Maybe a)    → Prism s s a b

-- The EMBEDDING FUNCTION is where you specify how to reverse your pattern match by embedding the prism's focus back into the pattern.
-- This is the function which will be called when reviewing and it must never fail.

-- The MATCHING FUNCTION determines whether the prism matches or fails.
-- For polymorphic prisms where `s` is not equal to `t` the match function must return a member of the type which matches the new type `t`.
-- This allows type changing traversals to work even when the prism fails to match.
-- For simpler prisms it's sufficient to provide Just the focus, or Nothing if the match fails.

_Just' ∷ Prism (Maybe a) (Maybe b) a b
_Just' = prism embed match
  where
    embed ∷ b → Maybe b
    embed = Just

    match ∷ Maybe a → Either (Maybe b) a
    match (Just a) = Right a
    match Nothing = Left Nothing

-- Since the _Nothing prism doesn't focus the type variable we know it can't be a polymorphic prism.
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

-- We want to match strings starting with a fixed prefix, and ignore ones that don't.
_Prefix ∷ String → Prism' String String
_Prefix prefix = prism' embed match
  where
    embed ∷ String → String
    embed s = prefix <> s

    match ∷ String → Maybe String
    match = stripPrefix prefix

-- When we run a prism we strip away the part of the data that we matched.
-- The idea is that we should be able to match and then embed and end up back where we started.
-- If we simply returned the original string without stripping the prefix first we'd end up with a duplicated prefix after making the round trip!

-- |
-- >>> "http://phishingscam.com" ^? _Prefix "https://"
-- Nothing

-- |
-- >>> "https://totallylegit.com" ^? _Prefix "https://"
-- Just "totallylegit.com"

-- We can even define new prisms using our existing one.
-- Only add our account number if the connection is secure!

-- |
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

-- Notice how we `divide out` the number when we match.
-- This allows us to properly chain our matches and discover more factors!

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
runFizzBuzz = for_ [1 .. 20] $ \n → putStrLn $ show n <> "\t" <> prismFizzBuzz n

-- 1	1
-- 2	2
-- 3	Fizz
-- 4	4
-- 5	Buzz
-- 6	Fizz
-- 7	7
-- 8	8
-- 9	Fizz
-- 10	Buzz
-- 11	11
-- 12	Fizz
-- 13	13
-- 14	14
-- 15	FizzBuzz
-- 16	16
-- 17	17
-- 18	Fizz
-- 19	19
-- 20	Buzz

-------------------------------
-- Exercises - Custom Prisms --
-------------------------------

-- 1. Try to write a custom prism for matching on the tail of a list:

-- _Tail ∷ Prism' [a] [a]

-- Is this possible? Why or why not?

-- Can't be implemented. We can't write an embed function for `_Tail`.

-- 2. Implement `_Cons` for lists using `prism`:

_ListCons ∷ Prism [a] [b] (a, [a]) (b, [b])
_ListCons = prism embed match
  where
    embed ∷ (b, [b]) → [b]
    embed (x, xs) = x : xs

    match ∷ [a] → Either [b] (a, [a])
    match [] = Left []
    match (x : xs) = Right (x, xs)

-- 3. BONUS: Implement `_Cycles` which detects whether a list consists of exactly 'n' repetitions of a pattern.

_Cycles ∷ (Eq a) ⇒ Int → Prism' [a] [a]
_Cycles n = prism' embed match
  where
    embed ∷ [a] → [a]
    embed = concat . replicate n

    match ∷ (Eq a) ⇒ [a] → Maybe [a]
    match xs =
      let chunk = take (length xs `div` n) xs
       in if concat (replicate n chunk) == xs then Just chunk else Nothing

-- `_Cycles` should behave as follows:

-- |
-- Find the subsequence which has been repeated exactly 3 times.
-- >>> "dogdogdog" ^? _Cycles 3
-- Just "dog"

-- |
-- The input isn't exactly 3 cycles of some input (it's 4), so we don't match.
-- >>> "dogdogdogdog" ^? _Cycles 3
-- Nothing

-- |
-- The input is exactly 3 cycles of 'a'.
-- >>> "aaa" ^? _Cycles 3
-- Just "a"

-- |
-- The input isn't a cycle at all.
-- >>> "xyz" ^? _Cycles 3
-- Nothing

-- |
-- We can review to create cycles.
-- >>> _Cycles 3 # "dog"
-- "dogdogdog"

-- |
-- We can mutate the sequence that's cycled.
-- >>> "dogdogdog" & _Cycles 3 .~ "cats"
-- "catscatscats"

-- Can you implement a version of _Cycles which doesn't depend on a specific iteration count? Why or why not?
-- No! Can't formulate an embedding function.

----------
-- Laws --
----------

-- Prism is a reversible pattern match.

-----------------------------
-- Law One: Review-Preview --
-----------------------------

-- This law entails the notion that review embeds values into the same pattern the prism matches.
-- If we review a value through a prism, then preview it through the same prism we should end up back where we started (with an extra Just).
-- First embed then match!

-- preview p (review p value) == Just value
-- (p # value) ^? p == Just value

-- |
-- >>> review _Left "Jabberwocky"
-- Left "Jabberwocky"

-- |
-- >>> _Left # "Jabberwocky"
-- Left "Jabberwocky"

-- |
-- >>> preview _Left (review _Left "Jabberwocky") == Just "Jabberwocky"
-- True

-- |
-- >>> preview _Just (review _Just "Jabberwocky") == Just "Jabberwocky"
-- True

-- |
-- >>> (_Just # "Jabberwocky") ^? _Just == Just "Jabberwocky"
-- True

-------------------------------
-- Law Two: Prism Complement --
-------------------------------

-- This law is effectively the inverse of the previous law.
-- First match then embed!

-- If we preview with a matching prism to get the focus 'a' ...
-- let Just a = preview myPrism s

-- and then 'review' that focus 'a' ...
-- let s' = review myPrism a

-- then we must end up back where we started.
-- s == s'

-- |
-- >>> let s = Right 32
-- >>> let Just a = preview _Right s  -- calls the matching function
-- >>> let s' = review _Right a       -- calls the embed function
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
-- >>> let Just a = preview (_Show ∷ Prism' String [Int]) s
-- >>> a
-- [1,2,3]

-- |
-- _Show has normalized the value to have no spaces between values.
-- >>> let s = "[1, 2, 3]"
-- >>> let Just a = preview (_Show ∷ Prism' String [Int]) s
-- >>> let s' = review _Show a
-- >>> s'
-- "[1,2,3]"

-- In practice we don't care. The two values are equal with respect to their semantic meaning.

---------------------------------------
-- Law Three: Pass-through Reversion --
---------------------------------------

-- `matching` combinator
-- matching ∷ Prism s t a b → s → Either t a

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

----------------------------
-- Exercises - Prism Laws --
----------------------------

-- 1. Implement the following prism and determine whether it's lawful:

_Contains ∷ ∀ a. Ord a ⇒ a → Prism' (Set a) (Set a)
_Contains x = prism' embed match
  where
    embed ∷ Set a → Set a
    embed = S.insert x

    match ∷ Set a → Maybe (Set a)
    match s = if x `elem` s then Just (S.delete x s) else Nothing

-- It should match on sets which contain the provided element! Reviewing adds the element to the set.
-- It should behave something like this:

-- |
-- >>> S.fromList [1, 2, 3] ^? _Contains 2
-- Just (fromList [1,3])

-- |
-- >>> S.fromList [1, 2, 3] ^? _Contains 10
-- Nothing

-- |
-- >>> _Contains 10 # S.fromList [1, 2, 3]
-- fromList [1,2,3,10]

-- |
-- >>> _Contains 2 # S.fromList [1, 2, 3]
-- fromList [1,2,3]

-- Is it lawful? Why or why not?

-- |
-- Law One
-- >>> (_Contains 10 # S.fromList [1, 2, 3]) ^? _Contains 10 == Just (S.fromList [1, 2, 3])
-- True

-- |
-- Law One
-- >>> (_Contains 2 # S.fromList [1, 2, 3]) ^? _Contains 2 == Just (S.fromList [1, 2, 3])
-- False

-- → Unlawful!!!

-- 2. Is the following prism lawful? Write out the checks to confirm your suspicions.

_Singleton ∷ ∀ a. Prism' [a] a
_Singleton = prism' embed match
  where
    embed ∷ a → [a]
    embed a = [a]

    match ∷ [a] → Maybe a
    match [a] = Just a
    match _ = Nothing

-- |
-- Law One
-- >>> (_Singleton # [1, 2, 3]) ^? _Singleton == Just [1, 2, 3]
-- True

-- |
-- Law One
-- >>> (_Singleton # []) ^? _Singleton == Just []
-- True

-- |
-- Law Two
-- >>> let s = [1]
-- >>> let Just a = preview _Singleton s
-- >>> let s' = review _Singleton a
-- >>> s == s'
-- True

-- → Lawful!!!

-------------------------------
-- Case Study: Simple Server --
-------------------------------

-- Every possible request has a path.
-- Let's build a lens for getting and setting the path no matter the type of request.
path ∷ Lens' Request Path
path = lens getter setter
  where
    getter ∷ Request → Path
    getter (Post p _) = p
    getter (Get p) = p
    getter (Delete p) = p

    setter ∷ Request → Path → Request
    setter (Post _ body) p = Post p body
    setter (Get _) p = Get p
    setter (Delete _) p = Delete p

-- |
-- >>> Post ["posts", "12345"] "My new post" ^. path
-- ["posts","12345"]

-- |
-- >>> Get ["posts", "12345"] ^. path
-- ["posts","12345"]

-- |
-- >>> Get ["posts", "12345"] & path .~ ["hello"]
-- Get ["hello"]

-- |
-- >>> Delete ["posts", "12345"] ^. path
-- ["posts","12345"]

-- |
-- >>> Delete ["posts", "12345"] & path .~ ["hello"]
-- Delete ["hello"]

-- default server handler
serveRequest ∷ Request → String
serveRequest _ = "404 Not Found"

-- |
-- >>> serveRequest $ Post ["hello"] "Is anyone there?"
-- "404 Not Found"

-- |
-- >>> serveRequest $ Get ["hello"]
-- "404 Not Found"

-- |
-- >>> serveRequest $ Delete ["user"]
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

-- NOTE!!! We could have implemented _PathPrefix much easier using the existing `prefixed` prism from the Lens library, but I wanted to show how it works behind the scenes.

------------------------------------
-- Altering Sub-Sets of Functions --
------------------------------------

-- |
-- >>> tail [1, 2, 3]
-- [2,3]

-- |
-- >>> tail []
-- *** Exception: Prelude.tail: empty list

-- a prism combinator
-- outside ∷ Prism s t a b → Lens (t → r) (s → r) (b → r) (a → r)

-- `outside` lifts a prism so that it matches on the argument of a function and returns a special lens.
-- This lens focuses on a portion of the provided function, specifically the part of the function which runs when the argument matches the prism.

-- When applied to a prism, the `outside` combinator returns a lens which focuses the subset of a function
-- which runs when the argument matches the prism, leaving the rest of the function untouched.

-- We pass in the original `tail` function as the value we're modifying, i.e. we are modifying a function.
-- Then we focus the portion of the function which deals with the empty list by using `outside _Empty`.
-- We replace that portion of the function with one that always returns [].
-- In other words: if the argument for `tail` is the empty list, then change `tail` to the function `const []`. In all other cases leave `tail` alone.
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
-- >>> server $ Get ["users", "12345"]
-- "User handler! Remaining path: 12345"

-- |
-- >>> server $ Delete ["users", "12345", "profile"]
-- "User handler! Remaining path: 12345/profile"

-- |
-- It should run the default handler for non user paths.
-- >>> server $ Post ["admin"] "DROP TABLE users"
-- "404 Not Found"

-- |
-- >>> server $ Get ["posts"]
-- "Posts Handler!"

-- |
-- >>> server $ Get ["posts", "index"]
-- "Post Index"

---------------------------
-- Matching on HTTP Verb --
---------------------------

-- We can access specific fields of our request type in a perfectly type-safe way!
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
-- >>> server' $ Get ["posts", "12345"]
-- "Fetching post at path: 12345"

-- |
-- >>> server' $ Post ["posts", "12345"] "My new post"
-- "Created post with body: My new post"

-- |
-- >>> server' $ Delete ["posts", "12345"]
-- "Deleting post at path: 12345"

-- This is the equivalent code using classic Haskell pattern matching:
postsHandler'' ∷ Request → String
postsHandler'' (Post path' body) = "Created post with body: " <> body
postsHandler'' (Get path') = "Fetching post at path: " <> intercalate "/" path'
postsHandler'' (Delete path') = "Deleting post at path: " <> intercalate "/" path'

-- This standard code is much more readable, easier to understand, doesn't need a "default case", and
-- will automatically be checked for completeness (if you've told GHC to check for incomplete pattern matches).

-- So when should you use prisms?

-- 1. Prisms are composable: doing multiple pattern matches in standard Haskell requires nesting function calls or case expressions

-- 2. Prisms interoperate with the rest of optics, providing a lot of flexibility and expressiveness.
--    You can perform pattern matching inside of an optics chain!

--------------------------------------------------------------------------------------------
--                                          Isos                                          --
--------------------------------------------------------------------------------------------

--------------------------
-- Introduction to Isos --
--------------------------

-- An isomorphism is a COMPLETELY REVERSIBLE transformation between two types or formats.

-----------------------------------------
-- How do Isos Fit into the Hierarchy? --
-----------------------------------------

--            |    Get   | Set/Modify | Traverse | Embed
---------------------------------------------------------
-- Lens       |  Single  |   Single   |  Single  |   ✗
-- Fold       |   Many   |     ✗      |    ✗     |   ✗
-- Traversal  |   Many   |   Many     |   Many   |   ✗
-- Prism      | Zero/One |  Zero/One  | Zero/One |   ✓
-- Iso        |  Single  |   Single   |  Single  |   ✓

-- Because Isos are completely reversible they're the strongest of all the optics we've seen.
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

-- An isomorphism is just a pair of functions where each function is the inverse of the other.

-- uncurry . curry == id
-- curry . uncurry == id

-- uncurry . curry == curry . uncurry

-- T.pack . T.unpack == id
-- T.unpack . T.pack == id

-- T.pack . T.unpack == T.unpack . T.pack

-- generally
-- to . from == from . to

-------------------
-- Building Isos --
-------------------

-- An Iso is an optic which views data after running its transformation on it.
-- We can view, modify or traverse the data in it's altered form!
-- If we modify the data in its altered form, the iso will convert the result back through the iso into the original form.
-- E.g. we can use an iso to edit a String as though it were a Text, and end up with a String again after the modification.

-- We convert the data through the Iso, run the modification, then convert it back.

--         to       from
--          |         |
-- iso ∷ (s → a) → (b → t) → Iso s t a b

packed ∷ Iso' String Text
packed = iso to from
  where
    to ∷ String → Text
    to = T.pack

    from ∷ Text → String
    from = T.unpack

-- |
-- Using `packed` views Strings as though they were Text (String → Text).
-- >>> ("Ay, caramba!" ∷ String) ^. packed
-- "Ay, caramba!"

-- |
-- `review`ing an iso runs its inverse (Text → String).
-- Here we use Iso as a Prism. Reviewing an Iso runs its inverse.
-- So we go from Text to String when reviewing `packed`.
-- packed # ("Suffering Succotash" ∷ Text)
-- "Suffering Succotash" ∷ String

-------------------------------
-- Flipping Isos with `from` --
-------------------------------

-- `from` turns an iso backwards:
-- We can use from to flip existing Isos!

-- from ∷ Iso  s t a b → Iso  b a t s
-- from ∷ Iso' s   a   → Iso'   a   s      -- simpler form

-- |
-- String → Text
-- >>> ("Good grief" ∷ String) ^. packed
-- "Good grief"

-- |
-- Text → String
-- ("Good grief" ∷ Text) ^. from packed
-- "Good grief" ∷ String

-- Using `from` to inverse/flip an Iso.
unpacked ∷ Iso' Text String
unpacked = from packed

-- Note: Both `packed` and `unpacked` are available in Data.Text.Lens from the `lens` library.

------------------------------------
-- Modification under Isomorphism --
------------------------------------

-- |
-- Using the awesome `replace` function from Text.
-- No need to implement a replace function for Strings.
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
-- (We could just use `T.toUpper` instead, but this demonstrates the point.)
-- let txt = "Lorem ipsum" ∷ Text
-- txt & from packed . traversed %~ toUpper
-- "LOREM IPSUM"

-- |
-- let txt = "Lorem ipsum" ∷ Text
-- txt & unpacked . traversed %~ toUpper
-- "LOREM IPSUM"

-------------------------------
-- Varieties of Isomorphisms --
-------------------------------

-- Many isomorphisms are trivial but still useful.
-- Things like converting between encodings, text formats, strict/lazy representations.
-- Or even conversions between data structures with different performance characteristics (e.g. Linked List/Vector).

-- There are interesting isomorphisms that don't even change the type of the data!
reversed' ∷ Iso' [a] [a]
reversed' = iso reverse reverse

-- `involuted` is helper for building isomorphisms where a function is its own inverse.
-- involuted ∷ (a → a) → Iso' a a
-- involuted f = iso f f

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
-- >>> [1, 2, 3] & reversed'' %~ take 2
-- [2,3]

-- |
-- We gain a lot of power by combining isos with all the other combinators we've learned.
-- >>> [1, 2, 3, 4] ^.. reversed'' . takingWhile (> 2) traversed
-- [4,3]

-- |
-- >>> "Blue suede shoes" & reversed'' . taking 1 worded .~ "gloves"
-- "Blue suede sevolg"

-- |
-- >>> "Blue suede shoes" & reversed'' . taking 1 worded . reversed'' .~ "gloves"
-- "Blue suede gloves"

-- swap values in a tuple
-- swapped ∷ Iso' (a, b) (b, a)

-- |
-- `swapped` lets us view a tuple as though the slots were swapped around.
-- >>> ("Fall","Pride") ^. swapped
-- ("Pride","Fall")

-- The real type of swapped is actually even more general.
-- It's backed by a Swapped typeclass and works on a lot of different Bifunctors.
-- swapped ∷ (Bifunctor p, Swapped p) ⇒ Iso (p a b) (p c d) (p b a) (p d c)

-- |
-- >>> Right "Field" ^. swapped
-- Left "Field"

-- flip function arguments, flip is its own inverse
-- flipped ∷ Iso' (a → b → c) (b → a → c)

-- |
-- >>> let (++?) = (++) ^. flipped
-- >>> "A" ++? "B"
-- "BA"

-- curry and uncurry function arguments
-- curried ∷   Iso' ((a, b) → c) (a → b → c)
-- uncurried ∷ Iso' (a → b → c) ((a, b) → c)

-- |
-- >>> let addTuple = (+) ^. uncurried
-- >>> addTuple (1, 2)
-- 3

-- |
-- >>> 10 ^. negated
-- -10

-- |
-- (-30 + 10) * (-1)
-- >>> over negated (+ 10) 30
-- 20

-- |
-- >>> 100 ^. adding 50
-- 150

-- |
-- >>> 100.0 ^. dividing 10
-- 10.0

-- |
-- Be careful of division by 0 for 'dividing'.
-- 100.0 ^. dividing 0
-- Numeric.Lens.dividing: divisor 0

-- |
-- Be careful of multiplying by 0 for 'multiplying'!
-- 100.0 ^. multiplying 0
-- Numeric.Lens.multiplying: factor 0

--------------------
-- Composing Isos --
--------------------

-- Isos compose just like any other optic! They compose both the 'forwards' and 'reversed' transformations.

-- |
-- Text → Text
-- let txt = "Winter is coming" ∷ Text
-- txt ^. unpacked . reversed
-- "gnimoc si retniW"

-- |
-- Text → Text
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

-------------------------------
-- Exercises - Intro to Isos --
-------------------------------

-- 1. For each of the following tasks, choose whether it's best suited to a Lens, Traversal, Prism, or Iso:

-- Focus a Celsius temperature in Fahrenheit.              → Iso: the conversion is reversible.
-- Focus the last element of a list.                       → Traversal: we're selecting a portion of a larger structure which might be missing.
-- View a JSON object as its corresponding Haskell Record. → Prism: We can always build valid JSON from a record, but may fail to construct a record from the json.
-- Rotate the elements of a three-tuple one to the right.  → Iso: We can reverse it by rotating back to the left or rotating twice more to the right.
-- Focus on the ‘bits’ of an Int as Bools.                 → Traversal or Prism: Either of Traversal' Int Bool or Prism [Bool] Int would be reasonable.
-- Focusing an IntSet from a Set Int.                      → Iso: The conversion is trivially reversible and lossless.

-- 2. Fill in the blanks!

-- |
--     ("Beauty", "Age") ^. _
-- >>> ("Beauty", "Age") ^. swapped
-- ("Age","Beauty")

-- |
--     50 ^. _ (adding 10)
-- >>> 50 ^. from (adding 10)
-- 40

-- |
--     0 & multiplying _ +~ 12
-- >>> 0 & multiplying 4 +~ 12
-- 3.0

-- |
--     0 & adding 10 . multiplying 2 .~ _
-- >>> 0 & adding 10 . multiplying 2 .~ 24
-- 2.0

-- |
--     [1, 2, 3] & reversed %~ _
-- >>> [1, 2, 3] & reversed %~ tail
-- [1,2]

-- |
--     (view _       (++)) [1, 2] [3, 4]
-- >>> (view flipped (++)) [1, 2] [3, 4]
-- [3,4,1,2]

-- |
--     [1, 2, 3] _ reversed
-- >>> [1, 2, 3] ^. reversed
-- [3,2,1]

-- BONUS: Hard ones ahead!

-- |
-- Note: transpose flips the rows and columns of a nested list:
-- >>> transpose [[1, 2, 3], [10, 20, 30]]
-- [[1,10],[2,20],[3,30]]

-- |
--     [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ _
-- >>> [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ tail
-- [[2,3],[20,30]]

-- Extra hard: use `switchCase` somehow to make this statement work:

-- |
--     (32, "Hi") & _2 . _                           .~ ("hELLO" ∷ String)
-- >>> let switchCase c = if isUpper c then toLower c else toUpper c
-- >>> (32, "Hi") & _2 . involuted (fmap switchCase) .~ ("hELLO" ∷ String)
-- (32,"Hello")

-- 3. You can convert from Celsius to Fahrenheit using the following formula:

celsiusToF ∷ Double → Double
celsiusToF c = (c * (9 / 5)) + 32

fahrenheitToC ∷ Double → Double
fahrenheitToC f = (f - 32) * (5 / 9)

-- Implement the following Iso.

fahrenheit' ∷ Iso' Double Double
fahrenheit' = iso to from
  where
    to = celsiusToF
    from = fahrenheitToC

---------------------
-- Projecting Isos --
---------------------

-- Since they're reversible and guaranteed to succeed, we can sneakily lift an iso over the contents of any Functor by exploiting `fmap`!
-- mapping' ∷ Functor f ⇒ Iso' s a → Iso' (f s) (f a)
-- mapping' iso' = iso (fmap $ view iso') (fmap $ review iso')

-- `mapping` let us easily transform nested portions of structures in our optics path as we go along!

toYamlList ∷ [String] → String
toYamlList xs = "- " <> intercalate "\n- " xs

-- |
-- >>> toYamlList ["Milk", "Eggs","Flour"]
-- "- Milk\n- Eggs\n- Flour"

-- |
-- Lift an Iso from [Text] to [String].
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
-- traverseOf_ (mapping unpacked . to toYamlList) putStrLn shoppingList
-- "- Milk\n- Eggs\n- Flour"

-- We can lift Isos through functors, e.g. from [String] → String to [Text] → Text
-- This applies to contravariant functors, bifunctors and even profunctors too!
-- The functions are called: `contramapping`, `bimapping`, `dimapping`.

-- We can contramap over the input and map over the output all in one go using dimapping!
-- We take the argument, map and unpack, then run our function. Lastly we pack the functions result.
textToYamlList ∷ [Text] → Text
textToYamlList = toYamlList ^. dimapping (mapping unpacked) packed

-- This is the simple version. :-)
textToYamlList' ∷ [Text] → Text
textToYamlList' = T.pack . toYamlList . fmap T.unpack

--------------------------------
-- Exercises - Projected Isos --
--------------------------------

-- 1. Fill in the blank!

-- |
--     ("Beauty", "Age") ^. mapping reversed . _
-- >>> ("Beauty", "Age") ^. mapping reversed . swapped
-- ("egA","Beauty")

-- |
--     [True, False, True] ^. mapping (_         not)
-- >>> [True, False, True] ^. mapping (involuted not)
-- [False,True,False]

-- |
--     [True, False, True] & _                       %~ filter id
-- >>> [True, False, True] & mapping (involuted not) %~ filter id
-- [False]

-- |
--     (show ^. _                          ) 1234
-- >>> (show ^. mapping reversed) 1234
-- "4321"

-- 2. Using the `enum` Iso provided by `lens`

-- enum ∷ Enum a ⇒ Iso' Int a

-- implement the following `intNot` function using `dimapping` in your implementation.

intNot ∷ Int → Int
intNot = fromEnum . not . toEnum

intNot' ∷ Int → Int
intNot' = not ^. dimapping enum (from enum)

intNot'' ∷ Int → Int
intNot'' = enum %~ not

-- |
-- >>> intNot 0
-- 1

-- |
-- >>> intNot 1
-- 0

-- |
-- intNot 2
-- Prelude.Enum.Bool.toEnum: bad argument

-----------------------
-- Isos and Newtypes --
-----------------------

------------------------
-- Coercing with Isos --
------------------------

-- We know about newtypes that the newtype is exactly equivalent to the underlying representation.
-- This means we have a trivial isomorphism between any type and any newtypes over that type.

-- import Data.Coerce (coerce)
-- :t coerce
-- coerce ∷ Coercible a b ⇒ a → b

-- Coercible is a special constraint, it's implemented for us by GHC for any newtype, we can use it even though it won't show up in instance lists.

newtype Email = Email String
  deriving (Show)

newtype UserID = UserID String
  deriving (Show)

-- Each of these types are representationally equal to Strings; the only difference is the type!
-- We can use `coerce` to convert between them.

-- |
-- We need to specify which type to coerce into.
-- >>> coerce ("joe@example.com" ∷ String) ∷ Email
-- Email "joe@example.com"

-- If two types are representationally equal we can even skip the middle-man and go directly between two newtypes.
-- >>> coerce (Email "joe@example.com") ∷ UserID
-- UserID "joe@example.com"

-- `lens` provides a handy Iso for converting between newtypes called coerced.
-- coerced ∷ (Coercible s a, Coercible t b) ⇒ Iso s t a b

-- |
-- >>> over coerced (reverse ∷ String → String) (Email "joe@example.com") ∷ Email
-- Email "moc.elpmaxe@eoj"

-- |
-- Type inference really struggles, and we'll almost always need to add a lot of extra annotations.
-- >>> Email "joe@example.com" & (coerced ∷ Iso' Email String) . traversed %~ toUpper
-- Email "JOE@EXAMPLE.COM"

-- Using a little helper to avoid type annotations.
email ∷ Iso' Email String
email = coerced

-- |
-- >>> Email "joe@example.com" & email . traversed %~ toUpper
-- Email "JOE@EXAMPLE.COM"

-- `makeLenses` is smart enough to derive exactly this Iso if we define our newtype using record syntax.
newtype Mail = Mail {_mail ∷ String}
  deriving (Show)

makeLenses ''Mail

-- `makeLenses` will generate a `mail` Iso which behaves just like the one we would have manually defined!

-- |
-- >>> Mail "joe@example.com" & mail . traversed %~ toUpper
-- Mail {_mail = "JOE@EXAMPLE.COM"}

--------------------------
-- Newtype Wrapper Isos --
--------------------------

-- These isos are essentially restricted forms of coerced which only map between newtypes and their
-- unwrapped form, they won't transitively map directly between different wrappers. This means
-- they won't map between Email and UserID for instance. They also don't allow type-changing
-- transformations. These restrictions mean they tend to result in much better type-inference.

-- `makeWrapped` creates _Wrapped', _Unwrapped' and _Wrapping'.
makeWrapped ''Mail

-- |
-- >>> Mail "joe@example.com" & _Wrapped' %~ reverse
-- Mail {_mail = "moc.elpmaxe@eoj"}

-- |
-- >>> Mail "joe@example.com" & _Wrapping' Mail %~ reverse
-- Mail {_mail = "moc.elpmaxe@eoj"}

-- The author prefers to use an explicit Iso such as the one generated by `makeLenses`.
-- But for types in base this can be convenient, e.g. `_Wrapping' Sum`.

----------
-- Laws --
----------

-----------------------------------------
-- The One and Only Law: Reversibility --
-----------------------------------------

-- The one and only law of an iso is that we can completely reverse the transformation.

-- myIso . from myIso == id
-- from myIso . myIso == id

-- `id` is a valid optic which always focuses its whole argument. It is a valid Iso as well!

-- |
-- >>> view (reversed . from reversed) "Testing one two three"
-- "Testing one two three"

-- |
-- >>> view (from reversed . reversed) "Testing one two three"
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

--------------------------
-- Exercises - Iso Laws --
--------------------------

-- 1. The following Iso is unlawful. Provide a counter example which shows that it breaks the law!

mapList' ∷ Ord k ⇒ Iso' (M.Map k v) [(k, v)]
mapList' = iso M.toList M.fromList

-- |
-- >>> view (from mapList' . mapList') [(5, 5), (5, 5)]
-- [(5,5)]

-- |
-- >>> view (mapList' . from mapList') $ M.fromList [(5, 5), (5, 5)]
-- fromList [(5,5)]

-- 2. Is there a lawful implementation of the following Iso? If so, implement it, if not, why not?

nonEmptyList' ∷ Iso [a] [b] (Maybe (NE.NonEmpty a)) (Maybe (NE.NonEmpty b))
nonEmptyList' = iso NE.nonEmpty (maybe [] NE.toList)

-- 3. Is there a lawful implementation of an iso which sorts a list of elements? If so, implement it, if not, why not?

-- No, there is no lawful implementation.

-- |
-- Sorted and unsorted lists are NOT identical. Ordering matters!
-- >>> let l = [1,6,1,4,1,3,2,5]
-- >>> l == sort l
-- False

-- 4. What about the following Iso which pairs each element with an Int which remembers its original position in the list.
--    Is this a lawful Iso? Why or why not? If not, try to find a counter-example.

sorted ∷ (Ord a) ⇒ Iso' [a] [(Int, a)]
sorted = iso to from
  where
    to xs = sortOn snd $ zip [0 ..] xs
    from xs = snd <$> sortOn fst xs

-- This is a UNLAWFUIL Iso!

-- |
-- >>> view (sorted . from sorted) [1,6,1,4,1,3,2,5]
-- [1,6,1,4,1,3,2,5]

-- |
-- >>> view (sorted . from sorted) []
-- []

-- |
-- >>> sortOn snd $ zip [0 ..] [1,6,1,4,1,3,2,5]
-- [(0,1),(2,1),(4,1),(6,2),(5,3),(3,4),(7,5),(1,6)]

-- |
-- >>> view (from sorted . sorted) [(0,1),(2,1),(4,1),(6,2),(5,3),(3,4),(7,5),(1,6)]
-- [(0,1),(2,1),(4,1),(6,2),(5,3),(3,4),(7,5),(1,6)]

-- |
-- >>> view (from sorted . sorted) []
-- []

-- |
-- But!!! The `to` function rezips the list.
-- >>> [(9, 'a'), (8, 'b'), (7, 'c')] ^. from sorted . sorted
-- [(2,'a'),(1,'b'),(0,'c')]

--------------------------------------------------------------------------------------------
--                                     Indexed Optics                                     --
--------------------------------------------------------------------------------------------

------------------------------
-- What are Indexed Optics? --
------------------------------

-- The basic idea of indexed optics is that they allow you to accumulate information about your current focus as you dive deeper into an optics path.
-- This information could be anything that makes sense in the context of the optic.
-- But it's commonly used with indexed structures to indicate the location you’re currently focusing on.

-- |
-- >>> ["Summer", "Fall", "Winter", "Spring"] ^.. traversed
-- ["Summer","Fall","Winter","Spring"]

-- |
-- The itraversed traversal behaves identically to traversed, BUT it keeps track of the current index as it does so!
-- We can use it in place of traversed without noticing any difference.
-- >>> ["Summer", "Fall", "Winter", "Spring"] ^.. itraversed
-- ["Summer","Fall","Winter","Spring"]

-- |
-- toListOf = flipped ^..
-- >>> toListOf itraversed ["Summer", "Fall", "Winter", "Spring"]
-- ["Summer","Fall","Winter","Spring"]

-- |
-- The magic happens when we start using indexed actions!
-- >>> itoListOf itraversed ["Summer", "Fall", "Winter", "Spring"]
-- [(0,"Summer"),(1,"Fall"),(2,"Winter"),(3,"Spring")]

-- itoListOf ∷ IndexedFold i s a → s → [(i, a)]
-- It has an operator:
-- (^@..) ∷ s → IndexedFold i s a → [(i, a)]

-- It's very important to note that it's the action which adds the index to the result.
-- The index isn't part of the focus of any optics in the path.
-- We can compose optics together without worrying about passing the index manually.

-- Try adding `@` in the middle of your favourite operators to get the indexed action.
-- Every indexed action accepts an indexed optic.

-- action     | operator | indexed action | indexed operator (@)
-- -----------+----------+----------------+---------------------
-- toListOf   |   (^..)  |   itoListOf    |        (^@..)
-- over       |   (%∼)   |   iover        |        (%@∼)
-- traverseOf |   (%%∼)  |   itraverseOf  |       (%%@∼)
-- set        |   (.∼)   |   iset         |        (.@∼)
-- view       |   (^.)   |   iview        |        (^@.)

-- Behavior of `itraversed` for some different container types:

-- |
-- Maps
-- The index type of maps is the key, so we can get a list of all elements and their key.
-- >>> let agenda = M.fromList [("Monday", "Shopping"), ("Tuesday", "Swimming")]
-- >>> agenda ^@.. itraversed
-- [("Monday","Shopping"),("Tuesday","Swimming")]

-- |
-- Tuples
-- The index type of tuples is the first half of the tuple.
-- >>> (True, "value") ^@.. itraversed
-- [(True,"value")]

-- |
-- Trees
-- The index type of trees is a list of int's which indicates their location in the tree.
-- >>> let t = Node "top" [Node "left" [], Node "right" []]
-- >>> t ^@.. itraversed
-- [([],"top"),([0],"left"),([1],"right")]

-----------------------
-- Index Composition --
-----------------------

-- Indexes can compose alongside the optics, but it can be a bit less intuitive.

-- |
-- By default, the index of a path will be the index of the last optic in the path.
-- >>> let agenda = M.fromList [ ("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"]) ]
-- >>> agenda ^@.. itraversed . itraversed
-- [(0,"Shopping"),(1,"Yoga"),(0,"Brunch"),(1,"Food coma")]

-- |
-- If we end the path with a non-indexed optic (like traverse) we'll get an error.
-- When using indexed actions make sure that your path has the type you expect and ends with an indexed optic!
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
-- (<.)  → Use the index of the optic to the left.
-- (.>)  → Use the index of the optic to the right. This is how (.) already behaves.
-- (<.>) → Combine the indices of both sides as a tuple.

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
-- >>> take 4 $ agenda ^@.. itraversed <.> itraversed <.> itraversed
-- [(("Monday",(0,0)),'S'),(("Monday",(0,1)),'h'),(("Monday",(0,2)),'o'),(("Monday",(0,3)),'p')]

-- |
-- >>> let agenda = M.fromList [ ("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"]) ]
-- >>> take 4 $ agenda ^@.. itraversed . itraversed . itraversed
-- [(0,'S'),(1,'h'),(2,'o'),(3,'p')]

------------------------------
-- Custom Index Composition --
------------------------------

-- `icompose` composes any two indexed optics into a new indexed optic by combining the indexes together with a user-defined function.

-- Simplified signature; `IndexedOptic` doesn't exist;
-- it's a stand-in for any of the indexed optic types.
-- icompose ∷ (i → j → k) →
--            IndexedOptic i s t a b →
--            IndexedOptic j a b c d →
--            IndexedOptic k s t c d

-- We could use `icompose` to append the list index to the day of the week as a string.
showDayAndNumber ∷ String → Int → String
showDayAndNumber a b = a <> ": " <> show b

-- |
-- >>> let agenda = M.fromList [ ("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"]) ]
-- >>> agenda ^@.. icompose showDayAndNumber itraversed itraversed
-- [("Monday: 0","Shopping"),("Monday: 1","Yoga"),("Saturday: 0","Brunch"),("Saturday: 1","Food coma")]

-- This can be a bit clunky, a custom operator might help.
(.++) ∷ (Indexed String s t → r) → (Indexed String a b → s → t) → Indexed String a b → r
(.++) = icompose (\a b → a ++ ", " ++ b)

-- The hardest part about writing these custom operators is figuring out their type!
-- You can leave the type signature off and simply use the operator in an expression, then ask GHCi or the language server for the type!

populationMap ∷ Map String (Map String Int)
populationMap = M.fromList [("Canada", M.fromList [("Ottawa", 994837), ("Toronto", 2930000)]), ("Germany", M.fromList [("Berlin", 3748000), ("Munich", 1456000)])]

-- |
-- >>> populationMap ^@.. itraversed .++ itraversed
-- [("Canada, Ottawa",994837),("Canada, Toronto",2930000),("Germany, Berlin",3748000),("Germany, Munich",1456000)]

--------------------------------
-- Exercises - Indexed Optics --
--------------------------------

-- 1. Fill in the blanks!

-- |
--     M.fromList [("streamResponse", False), ("useSSL", True)] _    itraversed
-- >>> M.fromList [("streamResponse", False), ("useSSL", True)] ^@.. itraversed
-- [("streamResponse",False),("useSSL",True)]

-- |
--     (M.fromList [('a', 1), ('b', 2)], M.fromList [('c', 3), ('d', 4)]) ^@.. _
-- >>> (M.fromList [('a', 1), ('b', 2)], M.fromList [('c', 3), ('d', 4)]) ^@.. both . itraversed
-- [('a',1),('b',2),('c',3),('d',4)]

-- |
--     M.fromList [('a', (True, 1)), ('b', (False, 2))] ^@.. itraversed _  _1
-- >>> M.fromList [('a', (True, 1)), ('b', (False, 2))] ^@.. itraversed <. _1
-- [('a',True),('b',False)]

-- |
--     [ M.fromList [("Tulips", 5), ("Roses", 3)] , M.fromList [("Goldfish", 11), ("Frogs", 8)] ] ^@.. _
-- >>> [ M.fromList [("Tulips", 5), ("Roses", 3)] , M.fromList [("Goldfish", 11), ("Frogs", 8)] ] ^@.. itraversed <.> itraversed
-- [((0,"Roses"),3),((0,"Tulips"),5),((1,"Frogs"),8),((1,"Goldfish"),11)]

-- |
--     [10, 20, 30] & itraversed _   (+)
-- >>> [10, 20, 30] & itraversed %@~ (+)
-- [10,21,32]

-- |
-- _            itraversed (\i s → putStrLn (replicate i ' ' <> s)) ["one", "two", "three"]
-- itraverseOf_ itraversed (\i s → putStrLn (replicate i ' ' <> s)) ["one", "two", "three"]

-- Check this in the repl!
-- one
--  two
--   three

-- |
-- itraverseOf_ itraversed (\n s → putStrLn _                    ) ["Go shopping", "Eat lunch", "Take a nap"]
-- itraverseOf_ itraversed (\n s → putStrLn (show n <> ": " <> s)) ["Go shopping", "Eat lunch", "Take a nap"]

-- Check this in the repl!
-- 0: Go shopping
-- 1: Eat lunch
-- 2: Take a nap

------------------------
-- Filtering by Index --
------------------------

-- We can use `indices` to narrow down the focuses using a predicate on the index of our fold or traversal!
-- You simply give it a predicate to run on the index and it'll ignore any focuses that don't match.

-- |
-- Get list elements with an 'even' list-index:
-- >>> ['a'..'z'] ^.. itraversed . indices even
-- "acegikmoqsuwy"

-- |
-- >>> let ratings = M.fromList [ ("Dark Knight", 94) , ("Dark Knight Rises", 87) , ("Death of Superman", 92)]
-- >>> ratings ^.. itraversed . indices (has (prefixed "Dark"))
-- [94,87]

-- If we want to be more specific we can target an exact index using the `index` filter.
-- `index` works similarly to `indices`, but ignores any focus that doesn't have the exact index you specify.

-- |
-- >>> ['a'..'z'] ^? itraversed . index 10
-- Just 'k'

-- |
-- >>> let ratings = M.fromList [ ("Dark Knight", 94) , ("Dark Knight Rises", 87) , ("Death of Superman", 92)]
-- >>> ratings ^? itraversed . index "Death of Superman"
-- Just 92

-------------------------------
-- Exercises - Index Filters --
-------------------------------

-- 1. Given the following exercise schedule:

exercises ∷ M.Map String (M.Map String Int)
exercises =
  M.fromList
    [ ("Monday" {-   -}, M.fromList [("pushups", 10), ("crunches", 20)]),
      ("Wednesday" {--}, M.fromList [("pushups", 15), ("handstands", 3)]),
      ("Friday" {-   -}, M.fromList [("crunches", 25), ("handstands", 5)])
    ]


-- a) Compute the total number of crunches you should do this week.

-- |
-- >>> sumOf (traversed . itraversed . index "crunches") exercises
-- 45

-- b) Compute the number of reps you need to do across all exercise types on Wednesday.

-- |
-- >>> sumOf (itraversed . index "Wednesday" . traversed) exercises
-- 18

-- c) List out the number of pushups you need to do each day, you can use `ix` to help this time if you wish.

-- |
-- >>> exercises ^@.. itraversed <. itraversed . index "pushups"
-- [("Monday",10),("Wednesday",15)]

-- |
-- >>> exercises ^@.. itraversed <. ix "pushups"
-- [("Monday",10),("Wednesday",15)]

-- 2. Given the following board:

board =
  [ "XOO",
    ".XO",
    "X.."
  ]

-- a) Generate a list of positions alongside their (row, column) coordinates.

-- |
-- >>> board ^@.. itraversed <.> itraversed
-- [((0,0),'X'),((0,1),'O'),((0,2),'O'),((1,0),'.'),((1,1),'X'),((1,2),'O'),((2,0),'X'),((2,1),'.'),((2,2),'.')]

-- b) Set the empty square at (1, 0) to an 'X'. HINT: When using the custom composition operators you'll often need to introduce parenthesis to get the right precedence.

-- |
-- >>> board & (itraversed <.> itraversed) . index (1, 0) .~ 'X'
-- ["XOO","XXO","X.."]

-- c) Get the 2nd column as a list (e.g. "OX."). Try to do it using `index` instead of `indices`!

-- |
-- >>> board ^.. traversed . itraversed . index 1
-- "OX."

-- d) Get the 3rd row as a list (e.g. "X.."). Try to do it using `index` instead of `indices`! HINT: The precedence for this one can be tricky too.

-- |
-- >>> board ^.. (itraversed <. traverse) . index 2
-- "X.."

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

-- Individual squares in our grid be represented as X-Y coordinates of Positions.
data Position = I | II | III
  deriving (Show, Eq, Ord)

-- a sample board won by X
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

-- Normally we'd build a custom fold with `folding`.
-- So to build an IndexedFold we can use `ifolding`.

-- simple signature
-- ifolding ∷ Foldable f ⇒ (s → f (i, a)) → IndexedFold i s a

-- We simply have to project our focuses from the structure into a foldable container (e.g. a list).
-- The difference from `folding` is that we also provide an index of type `i` alongside each value.

-- Use a list comprehension to get the list of all coordinate pairs in the correct order, then zip them with all the slots in our board.
slotsFold ∷ IndexedFold (Position, Position) (Board a) a
slotsFold = ifolding $ zip [(x, y) | y ← [I, II, III], x ← [I, II, III]] . toList

-- |
-- column-first ordering
-- >>> [(x, y) | y ← [I, II, III], x ← [I, II, III]]
-- [(I,I),(II,I),(III,I),(I,II),(II,II),(III,II),(I,III),(II,III),(III,III)]

-- |
-- >>> testBoard ^@.. slotsFold
-- [((I,I),'X'),((II,I),'O'),((III,I),'X'),((I,II),'.'),((II,II),'X'),((III,II),'O'),((I,III),'.'),((II,III),'O'),((III,III),'X')]

-- |
-- filter indices where the Y coord is 'II' → row
-- >>> testBoard ^@.. slotsFold . indices ((== II) . snd)
-- [((I,II),'.'),((II,II),'X'),((III,II),'O')]

-- |
-- filter indices where the X coord is 'II' → column
-- >>> testBoard ^@.. slotsFold . indices ((== II) . fst)
-- [((II,I),'O'),((II,II),'X'),((II,III),'O')]

------------------------------
-- Custom IndexedTraversals --
------------------------------

-- I'd recommend implementing the Ixed typeclass so that we can use `ix` to access particular slots.
-- But I show you how to do it by building an IndexedTraversal.

-- Unfortunately, there isn't a helper method for building custom Traversals.
-- Just like regular Traversals we have to do this by hand.

{- ORMOLU_DISABLE -}

-- We define an IndexedTraversal just like a regular Traversal.
-- The only difference is that we use the `indexed` function to pass an index to the handler each time we use it.
-- Typically you can write a normal traversal, then just sprinkle `indexed` around wherever you call your handler,
-- pass in the index alongside each value, and everything works out.

-- Definition of a polymorphic indexed traversal with a tuple of positions as the index.
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

-- Actually, our `slotsTraversal` makes the `slotsFold` redundant, since every traversal is also a fold.

-- |
-- Every traversal is also a fold.
-- >>> testBoard ^@.. slotsTraversal
-- [((I,I),'X'),((II,I),'O'),((III,I),'X'),((I,II),'.'),((II,II),'X'),((III,II),'O'),((I,III),'.'),((II,III),'O'),((III,III),'X')]

-- With Traversals we can also set and modify slots.

-- |
-- Setting the whole second row to 'O'.
-- >>> testBoard & slotsTraversal . indices ((== II) . snd) .~ 'O'
-- Board 'X' 'O' 'X' 'O' 'O' 'O' '.' 'O' 'X'

-- |
-- Setting the whole second column to 'O'.
-- >>> testBoard & slotsTraversal . indices ((== II) . fst) .~ 'O'
-- Board 'X' 'O' 'X' '.' 'O' 'O' '.' 'O' 'X'

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

-- You can also write an indexed lens using the `ilens` helper; unlike the others it's pretty straight-forward.
--
-- ilens ∷ (s → (i, a)) → (s → b → t) → IndexedLens i s t a b
--
-- You simply provide the index type alongside the focus in your getter and `ilens` will wire it up correctly!

-------------------
-- Index Helpers --
-------------------

-- |
-- The `indexing` helper takes a normal optic and simply adds a numeric index alongside its elements.
-- ("hello" ∷ Text) ^@.. indexing each
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]

-- |
-- >>> ['a'..'c'] ^@.. itraversed
-- [(0,'a'),(1,'b'),(2,'c')]

-- |
-- We can re-map or edit the indexes of an optic using `reindexed`.
-- >>> ['a'..'c'] ^@.. reindexed (* 10) itraversed
-- [(0,'a'),(10,'b'),(20,'c')]

-- |
-- We can change index types!
-- >>> ['a'..'c'] ^@.. reindexed show itraversed
-- [("0",'a'),("1",'b'),("2",'c')]

-- `selfIndex` can be injected into a path to set the index of the path to the current value.

-- |
-- >>> [("Betty", 37), ("Veronica", 12)] ^@.. itraversed
-- [(0,("Betty",37)),(1,("Veronica",12))]

-- |
-- >>> [("Betty", 37), ("Veronica", 12)] ^@.. itraversed <. _2
-- [(0,37),(1,12)]

-- |
-- `selfIndex` copies a snapshot of the current focus into the index.
-- Useful when you need context from higher up in the structure to do some edits lower down.
-- >>> [("Betty", 37), ("Veronica", 12)] ^@.. itraversed . selfIndex <. _2
-- [(("Betty",37),37),(("Veronica",12),12)]

---------------------------------------
-- Exercises - Custom Indexed Optics --
---------------------------------------

-- 1. Write a sensible implementation for the following indexed fold!

pair ∷ IndexedFold Bool (a, a) a
pair = ifolding (\(x, y) → [(False, x), (True, y)])

-- Such that:

-- |
-- >>> ('a', 'b') ^@.. pair
-- [(False,'a'),(True,'b')]

-- Once you’ve done that; try writing it as an indexed traversal!

pair' ∷ IndexedTraversal Bool (a, a) (b, b) a b
pair' p (x, y) = (,) <$> indexed p False x <*> indexed p True y

-- |
-- >>> ('a', 'b') ^@.. pair'
-- [(False,'a'),(True,'b')]

-- 2. Use `reindexed` to provide an indexed list traversal which starts at `1` instead of `0`.

-- We can change index types!
-- >>> ['a'..'c'] ^@.. reindexed show itraversed
-- [("0",'a'),("1",'b'),("2",'c')]

oneIndexed ∷ IndexedTraversal Int [a] [b] a b
oneIndexed = reindexed (+ 1) traversed

-- It should behave like so:

-- |
-- >>> ['a'..'d'] ^@.. oneIndexed
-- [(1,'a'),(2,'b'),(3,'c'),(4,'d')]

-- If you want a challenge, try using selfIndex, <.> and `reindexed` to write a traversal indexed by the distance to the end of the list. e.g.

invertedIndex ∷ IndexedTraversal Int [a] [b] a b
invertedIndex = reindexed (\(l, i) → length l - i - 1) (selfIndex <.> itraversed)

-- |
-- >>> ['a'..'d'] ^@.. invertedIndex
-- [(3,'a'),(2,'b'),(1,'c'),(0,'d')]

-- 3. Build the following combinators using only compositions of other optics:

chars ∷ IndexedTraversal Int Text Text Char Char
chars = indexing each

-- |
-- ("banana" ∷ Text) ^@.. chars
-- [(0,'b'),(1,'a'),(2,'n'),(3,'a'),(4,'n'),(5,'a')]

-- This one's a thinker; index each character (except newlines) by their line and column numbers:

charCoords ∷ IndexedTraversal (Int, Int) String String Char Char
charCoords = lined <.> itraversed

-- |
-- >>> "line\nby\nline" ^@.. charCoords
-- [((0,0),'l'),((0,1),'i'),((0,2),'n'),((0,3),'e'),((1,0),'b'),((1,1),'y'),((2,0),'l'),((2,1),'i'),((2,2),'n'),((2,3),'e')]

-----------------------------
-- Index-Preserving Optics --
-----------------------------

-- Index preserving optics are just regular optics which pass-through any existing index in the path AUTOMATICALLY.

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
-- We can use explicit index composition operators.
-- >>> [('a', True), ('b', False), ('c', True)] ^@.. itraversed <. _1
-- [(0,'a'),(1,'b'),(2,'c')]

-- |
-- However, we can turn `_1` into an index preserving lens with e.g. `cloneIndexPreservingLens`!
-- Now the index 'passes-through' `_1'` to the end AUTOMATICALLY.
-- >>> let _1' = cloneIndexPreservingLens _1
-- >>> [('a', True), ('b', False), ('c', True)] ^@.. itraversed . _1'
-- [(0,'a'),(1,'b'),(2,'c')]

-- Obviously in this case it's quicker, easier, and clearer to just use (<.).
-- But if you’re exporting lenses for a library it can be helpful to know about this trick.

-- cloneIndexPreserving* methods: `cloneIndexPreservingLens`, `cloneIndexPreservingTraversal`, `cloneIndexPreservingSetter`
-- Unfortunately, there is no `cloneIndexPreservingFold`!

-- cloneIndexPreservingLens      ∷ Lens s t a b      → IndexPreservingLens s t a b
-- cloneIndexPreservingTraversal ∷ Traversal s t a b → IndexPreservingTraversal s t a b
-- cloneIndexPreservingSetter    ∷ Setter s t a b    → IndexPreservingSetter s t a b

-- `iplens` is identical to `lens` except the resulting lens will be index-preserving.
-- iplens ∷ (s → a) → (s → b → t) → IndexPreservingLens s t a b

--------------------------------------------------------------------------------------------
--                                 Dealing with Type Errors                               --
--------------------------------------------------------------------------------------------

----------------------------------------
-- Interpreting expanded optics types --
----------------------------------------

-- |
-- [(1, True), (2, False), (3, True)] & folded . _1 *~ 10
-- Could not deduce (Contravariant Identity)
--   arising from a use of ‘folded’
-- from the context: Num a
--   bound by the inferred type of it ∷ Num a ⇒ [(a, Bool)]
--   at /home/schmidh/Gitrepos/Optics-by-Example-Exercises/app/Main.hs:7369:2-55

-- |
-- >>> [(1, True), (2, False), (3, True)] & traversed . _1 *~ 10
-- [(10,True),(20,False),(30,True)]

----------------------
-- Type Error Arena --
----------------------

---------------------------------
-- First Foe: Level 1 Lenslion --
---------------------------------

-- |
-- >>> view _1 ('a', 2)
-- 'a'

-----------------------
-- Level 2 Tuplicant --
-----------------------

-- |
-- view ("abc", 123) _1
-- Couldn't match type ‘([Char], b0)’
--                with ‘(t → Const t t)
--                      → ((a0 → f0 b1) → s0 → f0 t0)
--                      → Const t ((a0 → f0 b1) → s0 → f0 t0)’
-- Expected type: Getting t ((a0 → f0 b1) → s0 → f0 t0) t
--   Actual type: ([Char], b0)

-- |
-- >>> view _1 ("abc", 123)
-- "abc"

-------------------------
-- Level 3 Settersiren --
-------------------------

-- |
-- ("old", False) $ _1 .~ "new"
-- Couldn't match expected type ‘(s0 → t0) → t’
--             with actual type ‘([Char], Bool)’

-- |
-- >>> ("old", False) & _1 .~ "new"
-- ("new",False)

-------------------------
-- Level 4 Composicore --
-------------------------

-- |
-- view (_3 . _1) (('a', 'b', 'c'), 'd')
-- No instance for (Field3
--                    ((Char, Char, Char), Char) ((Char, Char, Char), Char) a0 b0)
--   arising from a use of ‘it’

-- |
-- >>> view (_1 . _3) (('a', 'b', 'c'), 'd')
-- 'c'

-------------------------
-- Level 5 Foldasaurus --
-------------------------

-- |
-- ("blue", Just (2 ∷ Int)) ^. _2 . _Just
-- No instance for (Monoid Int) arising from a use of ‘_Just’

-- |
-- >>> ("blue", Just (2 ∷ Int)) ^? _2 . _Just
-- Just 2

-- |
-- >>> ("blue", Just (2 ∷ Int)) ^? _2
-- Just (Just 2)

--------------------------------
-- Level 6 Higher Order Beast --
--------------------------------

-- |
-- ['a'..'z'] ^.. taking 5 . folded
-- Couldn't match type ‘[]’ with ‘p a’
-- Expected type: Getting
--                  (Endo [BazaarT p f a a a]) [Char] (BazaarT p f a a a)
--   Actual type: (BazaarT p f a a a → s0 → BazaarT p f a a a)
--                → Over p f s0 (BazaarT p f a a a) a a

-- |
-- >>> ['a'..'z'] ^.. taking 5 folded
-- "abcde"

--------------------------
-- Level 7 Traversacula --
--------------------------

-- |
-- over both putStrLn ("one", "two")
-- No instance for (Show (IO ())) arising from a use of ‘evalPrint’

-- |
-- traverseOf_ both putStrLn ("one", "two")
-- one
-- two

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

-- using standard monad stack API
printUser ∷ ReaderT Env IO ()
printUser = do
  user ← asks _currentUser
  liftIO . putStrLn $ "Current user: " <> user

-- The equivalent lensy version just uses `view`.
-- This is actually the exact same `view` function we're used to using with lenses!
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

-- Using `view` is the idiomatic way of accessing your environment if you've got lenses defined for your environment.

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

-- Almost ALL setter combinators have State equivalents which simply replace the `∼` with an `=`, e.g `.~` becomes `.=`!

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
  num ← ask -- `num` can be any Float from any lens, e.g. `temp` and `pressure`.
  liftIO . putStrLn $ statName <> ": " <> show num

-- `magnify` is used for Reader monads.
-- magnify ∷ Lens' s a → ReaderT a m r → ReaderT s m r

weatherStats ∷ ReaderT Weather IO ()
weatherStats = do
  magnify temp (printData "temp") -- `magnify` et. al. allow us to ‘re-scope’ a Reader or State monad to a portion of the type focused by a lens.
  magnify pressure (printData "pressure") -- `temp` and `pressure` can use the same function (`printData`).

-- |
-- By magnifying the Reader environment through a lens which focuses a Float we can run `printData` against that particular stat!
-- runReaderT weatherStats (Weather 15 7.2)
-- temp: 15.0
-- pressure: 7.2

-- a State action which runs against our Weather object
convertCelsiusToFahrenheit ∷ StateT Float IO ()
convertCelsiusToFahrenheit = modify (\celsius → (celsius * (9 / 5)) + 32)

-- `zoom` is used for State monads.
-- zoom ∷ Monad m ⇒ Lens' s a → StateT a m r → StateT s m r

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
