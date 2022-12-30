-- {-# OPTIONS_GHC -ddump-splices #-}

module Lenses where

import Control.Lens
import Control.Lens.Unsound
import Data.Char

--------------------------------------------------------------------------------------------
--                                      3. Lenses                                         --
--------------------------------------------------------------------------------------------

--------------------------------
-- 3.1 Introduction to Lenses --
--------------------------------

-- Lenses have the following concrete guarantees:
-- 1. a Lens focuses (i.e. selects) a SINGLE piece of data within a larger structure.
-- 2. a Lens must NEVER FAIL to get or modify that focus.

-------------
-- Anatomy --
-------------

e011 ∷ String
e011 = view (_2 . _1) (42, ("hello", False))

-- |
-- >>> e011
-- "hello"

-- e012
e012 ∷ Integer
e012 = view (_1 . _2) ((1, 2), 3)

-- |
-- >>> e012
-- 2

-- e013
e013 ∷ (Bool, Either String c)
e013 = set (_2 . _Left) "new" (False, Left "old")

-- |
-- >>> e013
-- (False,Left "new")

-- e014
e014 ∷ String
e014 = over (taking 2 worded . traversed) toUpper "testing one two three"

-- |
-- >>> e014
-- "TESTING ONE two three"

-- e015
e015 ∷ String
e015 = foldOf (both . each) (["super", "cali"], ["fragilistic", "expialidocious"])

-- |
-- >>> e015
-- "supercalifragilisticexpialidocious"

-- |
-- Monoid instance shines trough, sometimes folding with view works, sometimes not! (p. 105, "Using `view` on Folds")
-- >>> (["super", "cali"], ["fragilistic", "expialidocious"]) ^. both . each
-- "supercalifragilisticexpialidocious"

-------------------------------
-- Exercises - Optic Anatomy --
-------------------------------

e016 ∷ Integer
e016 = view (_1 . _2) ((1, 2), 3)

-- |
-- >>> e016
-- 2

-- action: view
-- path: (_1 . _2)
-- structure: ((1, 2), 3)
-- focus: 2

e017 ∷ (Bool, Either [Char] c)
e017 = set (_2 . _Left) "new" (False, Left "old")

-- |
-- >>> e017
-- (False,Left "new")

-- action: set
-- path: (_2 . _Left)
-- structure: (False, Left "old")
-- focus: "old"

e018 ∷ String
e018 = over (taking 2 worded . traversed) toUpper "testing one two three"

-- |
-- >>> e018
-- "TESTING ONE two three"

-- action: over
-- path: (taking 2 worded . traversed)
-- structure: "testing one two three"
-- focus: "testing one"

e019 ∷ String
e019 = foldOf (both . each) (["super", "cali"], ["fragilistic", "expialidocious"])

-- |
-- >>> e019
-- "supercalifragilisticexpialidocious"

-- action: foldOf
-- path: (both . each)
-- structure: (["super", "cali"],["fragilistic", "expialidocious"])
-- focus: "super", "cali", "fragilistic", "expialidocious"

----------------------
-- 3.2 Lens Actions --
----------------------

----------------------------
-- Viewing through Lenses --
----------------------------

e020 ∷ Char
e020 = view _1 ('a', 'b')

-- |
-- >>> e020
-- 'a'

-- e021
e021 ∷ Char
e021 = view _2 ('a', 'b')

-- |
-- >>> e021
-- 'b'

----------------------------
-- Setting through a Lens --
----------------------------

e022 ∷ (Char, Char)
e022 = set _1 'x' ('a', 'b')

-- |
-- >>> e022
-- ('x','b')

-- e023
e023 ∷ (Integer, Integer)
e023 = over _1 (* 100) (1, 2)

-- |
-- >>> e023
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

e024 ∷ (Bool, Integer)
e024 = over _2 (* 10) (False, 2)

-- |
-- >>> e024
-- (False,20)

-----------------------------
-- 3.3. Lenses and Records --
-----------------------------

----------------------------------------
-- Building a Lens for a Record Field --
----------------------------------------

data Ship where
  Ship ::
    { _shipName ∷ String,
      _numCrew ∷ Int
    } ->
    Ship
  deriving (Show)

purplePearl ∷ Ship
purplePearl =
  Ship
    { _shipName = "Purple Pearl",
      _numCrew = 38
    }

makeLenses ''Ship

-- >>> :type numCrew
-- numCrew ∷ Functor f ⇒ (Int → f Int) → Ship → f Ship

-- >>> :type shipName
-- shipName ∷ Functor f ⇒ (String → f String) → Ship → f Ship

-- from splices dump (cleaned)
-- numCrew ∷ Lens' Ship Int
-- numCrew f (Ship amt x2) = fmap Ship amt (f x2)
--
-- shipName ∷ Lens' Ship String
-- shipName f (Ship amt x2) = fmap (`Ship` x2) (f amt)

-- from the book
-- `makeLenses` creates the following lenses using `lens`:
--
--                               Before structure type
--                                     |
--        Getter Fn  Setter Fn         | Before focus type
--           |           |             |   |
-- lens ∷ (s → a) → (s → b → t) → Lens s t a b
--                                       |   |
--                                       | After focus type
--                                       |
--                                 After structure type

-- >>> :type lens
-- lens ∷ Functor f ⇒ (s → a) → (s → b → t) → (a → f b) → s → f t

--
-- or the simpler type signature:
--
--        Getter Fn   Setter Fn      Structure type
--           |           |              |
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
e025 ∷ Int
e025 = view numCrew purplePearl

-- |
-- >>> e025
-- 38

-- |
-- The same using the field accessor.
-- >>> _numCrew purplePearl
-- 38

-- e026
e026 ∷ Ship
e026 = set numCrew 41 purplePearl

-- |
-- >>> e026
-- Ship {_shipName = "Purple Pearl", _numCrew = 41}

-- e027
e027 ∷ Ship
e027 = set shipName "More Purple Pearl" purplePearl

-- |
-- >>> e027
-- Ship {_shipName = "More Purple Pearl", _numCrew = 38}

----------------------------------
-- Modifying Fields with a Lens --
----------------------------------

e028 ∷ Ship
e028 = over numCrew (+ 3) purplePearl

-- |
-- >>> e028
-- Ship {_shipName = "Purple Pearl", _numCrew = 41}

-- e029
e029 ∷ Ship
e029 = set numCrew (view numCrew purplePearl + 3) purplePearl

-- |
-- The same using `view` and `set` and running a function over the viewed value.
-- >>> e029
-- Ship {_shipName = "Purple Pearl", _numCrew = 41}

-------------------------------------------
-- Automatically Generating Field Lenses --
-------------------------------------------

-- Automatically generated lenses by e.g. `makeLenses` can be seen in the repl with the browse command!

-- Prints out the "nice" names for our lenses!

-- :browse Main
-- ...
-- numCrew ∷ Lens' Ship Int
-- shipName ∷ Lens' Ship String
-- ...

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
-- data Pet where
--   Pet ∷
--     { _petName ∷ String,
--       _petType ∷ String
--     } →
--     Pet

-- getPetName ∷ Pet → String
-- getPetName pet = view petName pet

-- makeLenses ''Pet

-- The error says:
-- • Variable not in scope: petName ∷ Lens' Pet String

-- The `makeLenses` call should come right after the data definition of Pet because of TemplateHaskell staging.

---------------------
-- 3.4 Limitations --
---------------------

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

-------------------
-- 3.5 Lens Laws --
-------------------

--------------------
-- Case Study: _1 --
--------------------

-- `_1` is a lawful lens.

-- |
-- 1st lens law:
-- You get back what you set (SET-GET)
e030 ∷ Bool
e030 = view _1 (set _1 "Firefly" ("Star Wars", "Star Trek")) == "Firefly"

-- |
-- >>> e030
-- True

-- |
-- 2nd lens law:
-- Setting back what you got doesn't do anything (GET-SET)
-- No weird side-effects!
e031 ∷ Bool
e031 =
  let structure = ("Simpsons", "Seinfeld")
   in set _1 (view _1 structure) structure == structure

-- |
-- >>> e031
-- True

-- |
-- 3rd lens law:
-- Setting twice is the same as setting once (SET-SET)
e032 ∷ Bool
e032 =
  let value1 = "Coffee"
      value2 = "Red Bull"
      structure = ("Beer", "Tea")
   in set _1 value2 (set _1 value1 structure) == set _1 value2 structure

-- |
-- >>> e032
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
data Err where
  ReallyBadError ∷ {_msg ∷ String} → Err
  ExitCode ∷ {_code ∷ Int} → Err
  deriving (Eq)

-- makeLenses ''Err

msg ∷ Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg ∷ Err → String
    getMsg (ReallyBadError message) = message
    getMsg (ExitCode _) = "" -- Hrmm, I guess we just return "" ?
    setMsg ∷ Err → String → Err
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    setMsg (ExitCode n) newMessage = ExitCode n -- Nowhere to set it, I guess we do nothing?

-- |
-- Checking 1st lens law:
-- You get back what you set (set-get)

-- e033
e033 ∷ Bool
e033 =
  let newMessage = "False alarm!"
   in view msg (set msg newMessage (ExitCode 1)) == newMessage

-- |
-- Law is broken!
-- >>> e033
-- False

-- Checking 2nd lens law:
-- Setting back what you got doesn't do anything (get-set)

-- e034
e034 ∷ Bool
e034 =
  let err = ReallyBadError "BAD BAD BAD"
   in set msg (view msg err) err == err

-- |
-- >>> e034
-- True

-- e035
e035 ∷ Bool
e035 =
  let err = ExitCode 1
   in set msg (view msg err) err == err

-- |
-- >>> e035
-- True

-- |
-- Checking 3rd lens law:
-- Setting twice is the same as setting once (set-set)

-- e036
e036 ∷ Bool
e036 =
  let err = ReallyBadError "BAD BAD BAD"
      value1 = "Value1"
      value2 = "Value2"
   in set msg value2 (set msg value1 err) == set msg value2 err

-- |
-- >>> e036
-- True

-- e037
e037 ∷ Bool
e037 =
  let err = ExitCode 1
      value1 = "Value1"
      value2 = "Value2"
   in set msg value2 (set msg value1 err) == set msg value2 err

-- |
-- >>> e037
-- True

-----------------------------
-- Case Study: lensProduct --
-----------------------------

-- A very useful unlawful lens: lensProduct.
-- lensProduct ∷ Lens' s a → Lens' s b → Lens' s (a, b)

-- It allows you to take two lenses which accept the same structure to simultaneously focus two distinct
-- parts of it. That sounds reasonable enough, and indeed this situation comes up relatively often.

type UserName = String

type UserId = String

data Session where
  Session ::
    { _userId ∷ UserId,
      _userName ∷ UserName
    } ->
    Session
  deriving (Show, Eq)

makeLenses ''Session

userInfo ∷ Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

-- e038
e038 ∷ (UserId, UserName)
e038 =
  let session = Session "USER-1234" "Joey Tribbiani"
   in view userInfo session

-- |
-- >>> e038
-- ("USER-1234","Joey Tribbiani")

-- Session and `userId` are not disjoint - they overlap!
alongsideUserId ∷ Lens' Session (Session, UserId)
alongsideUserId = lensProduct id userId

-- changing the user in the new session
e039 ∷ (Session, UserId)
e039 =
  let session = Session "USER-1234" "Joey Tribbiani"
      newSession = session {_userId = "USER-5678"}
   in view alongsideUserId (set alongsideUserId (newSession, "USER-9999") session)

-- |
-- >>> e039
-- (Session {_userId = "USER-9999", _userName = "Joey Tribbiani"},"USER-9999")

-- |
-- 1st lens law
e040 ∷ Bool
e040 =
  let session = Session "USER-1234" "Joey Tribbiani"
      newSession = session {_userId = "USER-5678"}
   in view alongsideUserId (set alongsideUserId (newSession, "USER-9999") session) == (newSession, "USER-9999")

-- |
-- >>> e040
-- False

-- This time order has changed.
alongsideSession ∷ Lens' Session (UserId, Session)
alongsideSession = lensProduct userId id

-- value in the session overwrites the other
e041 ∷ (UserId, Session)
e041 =
  let session = Session "USER-1234" "Joey Tribbiani"
      newSession = session {_userId = "USER-5678"}
   in view alongsideSession (set alongsideSession ("USER-9999", newSession) session)

-- |
-- >>> e041
-- ("USER-5678",Session {_userId = "USER-5678", _userName = "Joey Tribbiani"})

-- e042
e042 ∷ Bool
e042 =
  let session = Session "USER-1234" "Joey Tribbiani"
      newSession = session {_userId = "USER-5678"}
   in view alongsideSession (set alongsideSession ("USER-9999", newSession) session) == ("USER-9999", newSession)

-- |
-- >>> e042
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

-- 1st law (set-get).
e043 ∷ Bool
e043 =
  let tstData = ([5, 4, 3, 2, 1], 5)
      newValue = 6
   in view recorder (set recorder newValue tstData) == newValue

-- |
-- >>> e043
-- True

-- 2nd law (get-set).
e044 ∷ Bool
e044 =
  let tstData = ([5, 4, 3, 2, 1], 5)
      newValue = 6
   in set recorder (view recorder tstData) tstData == tstData

-- |
-- >>> e044
-- False

-- 3rd law (set-set).
e045 ∷ Bool
e045 =
  let tstData = ([5, 4, 3, 2, 1], 5)
      newValue = 6
      anotherValue = 7
   in set recorder newValue (set recorder anotherValue tstData) == set recorder newValue tstData

-- |
-- >>> e045
-- False

-- 2. Test the get-set and set-set laws for the `msg` lens we wrote this chapter. Does it pass these laws? See above - they pass.

--------------------
-- Virtual Fields --
--------------------

-----------------------------
-- Writing a Virtual Field --
-----------------------------

data Temperature where
  Temperature ::
    { _location ∷ String,
      _celsius ∷ Float
    } ->
    Temperature
  deriving (Show)

makeLenses ''Temperature

-- celsius ∷ Lens' Temperature Float

-- e046
e046 ∷ Float
e046 =
  let temp = Temperature "Berlin" 7.0
   in view celsius temp

-- |
-- >>> e046
-- 7.0

-- e047
e047 ∷ Temperature
e047 =
  let temp = Temperature "Berlin" 7.0
   in set celsius 13.5 temp

-- |
-- >>> e047
-- Temperature {_location = "Berlin", _celsius = 13.5}

-- Bump the temperature up by 10 degrees Celsius
e048 ∷ Temperature
e048 =
  let temp = Temperature "Berlin" 7.0
   in over celsius (+ 10) temp

-- |
-- >>> e048
-- Temperature {_location = "Berlin", _celsius = 17.0}

-- Conversion Functions
celsiusToFahrenheit ∷ Float → Float
celsiusToFahrenheit c = (c * (9 / 5)) + 32

fahrenheitToCelsius ∷ Float → Float
fahrenheitToCelsius f = (f - 32) * (5 / 9)

-- e049
e049 ∷ Float
e049 =
  let temp = Temperature "Berlin" 7.0
   in view celsius temp & celsiusToFahrenheit

-- |
-- >>> e049
-- 44.6

-- e050
e050 ∷ Temperature
e050 =
  let temp = Temperature "Berlin" 7.0
   in set celsius (fahrenheitToCelsius 56.3) temp

-- |
-- >>> e050
-- Temperature {_location = "Berlin", _celsius = 13.5}

-- Bump the temp by 18 degrees Fahrenheit
e051 ∷ Temperature
e051 =
  let temp = Temperature "Berlin" 7.0
   in over celsius (fahrenheitToCelsius . (+ 18) . celsiusToFahrenheit) temp

-- |
-- >>> e051
-- Temperature {_location = "Berlin", _celsius = 17.0}

-- virtual field (using existing lens)
-- `Temperature` doesn't have a field for Fahrenheit. We fake it by using the `celsius` lens to create a virtual field!
fahrenheit ∷ Lens' Temperature Float
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter temp f = set celsius (fahrenheitToCelsius f) temp

-- e052
e052 ∷ Float
e052 =
  let temp = Temperature "Berlin" 7.0
   in view fahrenheit temp

-- |
-- >>> e052
-- 44.6

-- e053
e053 ∷ Temperature
e053 =
  let temp = Temperature "Berlin" 7.0
   in set fahrenheit 56.3 temp

-- |
-- >>> e053
-- Temperature {_location = "Berlin", _celsius = 13.5}

-- e054
e054 ∷ Temperature
e054 =
  let temp = Temperature "Berlin" 7.0
   in over fahrenheit (+ 18) temp

-- |
-- >>> e054
-- Temperature {_location = "Berlin", _celsius = 17.0}

--------------------------------
-- Exercises - Virtual Fields --
--------------------------------

data User where
  User ::
    { _firstName ∷ String,
      _lastName ∷ String,
      _userEmail ∷ String
    } ->
    User
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

-- e055
e055 ∷ String
e055 =
  let user = User "John" "Cena" "invisible@example.com"
   in view fullName' user

-- |
-- >>> e055
-- "John Cena"

-- e056
e056 ∷ User
e056 =
  let user = User "John" "Cena" "invisible@example.com"
   in set fullName' "Doctor of Thuganomics" user

-- |
-- >>> e056
-- User {_firstName = "Doctor", _lastName = "of Thuganomics", _userEmail = "invisible@example.com"}

----------------------------------------------------
-- 3.7 Data Correction and Maintaining Invariants --
----------------------------------------------------

------------------------------------------
-- Including Correction Logic in Lenses --
------------------------------------------

data Time where
  Time ::
    { _hours ∷ Int,
      _mins ∷ Int
    } ->
    Time
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

-- e057
e057 ∷ Time
e057 =
  let time = Time 3 10
   in set hours 40 time

-- |
-- >>> e057
-- Time {_hours = 23, _mins = 10}

-- e058
e058 ∷ Time
e058 =
  let time = Time 3 10
   in set mins (-10) time

-- |
-- >>> e058
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

-- e059
e059 ∷ Time
e059 =
  let time = Time 3 10
   in over mins' (+ 55) time

-- |
-- >>> e059
-- Time {_hours = 4, _mins = 5}

-- e060
e060 ∷ Time
e060 =
  let time = Time 3 10
   in over mins' (subtract 20) time

-- |
-- >>> e060
-- Time {_hours = 2, _mins = 50}

-- e061
e061 ∷ Time
e061 = over mins' (+ 1) (Time 23 59)

-- |
-- >>> e061
-- Time {_hours = 0, _mins = 0}

----------------------------------------
-- Exercises - Self-Correcting Lenses --
----------------------------------------

data ProducePrices where
  ProducePrices ::
    { _limePrice ∷ Float,
      _lemonPrice ∷ Float
    } ->
    ProducePrices
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

-- e062
e062 ∷ ProducePrices
e062 =
  let prices = ProducePrices 1.50 1.48
   in set limePrice 2 prices

-- |
-- >>> e062
-- ProducePrices {_limePrice = 2.0, _lemonPrice = 1.5}

-- e063
e063 ∷ ProducePrices
e063 =
  let prices = ProducePrices 1.50 1.48
   in set limePrice 1.8 prices

-- |
-- >>> e063
-- ProducePrices {_limePrice = 1.8, _lemonPrice = 1.48}

-- e064
e064 =
  let prices = ProducePrices 1.50 1.48
   in set limePrice 1.63 prices

-- |
-- >>> e064
-- ProducePrices {_limePrice = 1.63, _lemonPrice = 1.48}

-- e065
e065 ∷ ProducePrices
e065 =
  let prices = ProducePrices 1.50 1.48
   in set limePrice (-1.00) prices

-- |
-- >>> e065
-- ProducePrices {_limePrice = 0.0, _lemonPrice = 0.5}
