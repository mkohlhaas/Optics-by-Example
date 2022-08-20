module Main where

import Control.Applicative
import Control.Lens
import Control.Lens.Extras (biplate)
import Control.Lens.Unsound (lensProduct)
import Data.Bits.Lens (bitAt)
import Data.Char
import Data.List (sort)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Numeric.Lens (negated)

main ∷ IO ()
main = putStrLn "Hello Optics by Example!"

-- |
-- Update portions of immutable data structures
-- >>> set _3 False ('a', 'b', 'c')
-- ('a','b',False)

-- |
-- We can perform a task over deeply nested subsets of data.
-- Let's sum all numbers which in a 'Left' within the right half of each tuple
-- >>> sumOf (folded . _2 . _Left) [(True, Left 10), (False, Right "pepporoni"), (True, Left 20)]
-- 30

-- |
-- >>> stories = ["This one time at band camp", "Nuff!", "This is a short story"]
-- >>> over (traversed . filtered ((>10) . length)) (\story → take 10 story ++ "...") stories
-- ["This one t...","Nuff!","This is a ..."]

-- |
-- Summarize a list of numbers, subtracting the 'Left's, adding the 'Right's!
-- >>> sumOf (folded . beside negated id) [Left 1, Right 10, Left 2, Right 20]
-- 27

-- |
-- >>> "why is a raven like a writing desk" & worded . _head %~ toUpper
-- "Why Is A Raven Like A Writing Desk"

-- |
-- Multiply every Integer by 100 no matter where they are in the structure:
-- >>> (Just 3, Left ("hello", [13, 15, 17])) & biplate *~ 100
-- (Just 300,Left ("hello",[1300,1500,1700]))

-- |
-- Reverse the ordering of all even numbers in a sequence.
-- We leave the odd numbers alone!
-- >>> [1, 2, 3, 4, 5, 6, 7, 8] & partsOf (traversed . filtered even) %~ reverse
-- [1,8,3,6,5,4,7,2]

-- |
-- Sort all the characters in all strings, across word boundaries!
-- >>> ("one", "two", "three") & partsOf (each . traversed) %~ L.sort
-- ("eee","hno","orttw")

-- |
-- Flip the 2nd bit of each number to a 0
-- >>> [1 ∷ Int, 2, 3, 4] & traversed . bitAt 1 %~ not
-- [3,0,1,6]

-- Prompt the user with each question in a tuple, then return the tuple with each prompt replaced with the user's input.
-- prompts = ("What is your name?" , "What is your quest?" , "What is your favourite color?")
-- prompts & each %%~ (\prompt → putStrLn prompt >> getLine)

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

-- |
-- >>> view _1 ('a', 'b')
-- 'a'

-- |
-- >>> view _2 ('a', 'b')
-- 'b'

-- |
-- >>> set _1 'x' ('a', 'b')
-- ('x','b')

-- |
-- >>> over _1 (*100) (1, 2)
-- (100,2)

-- |
-- >>> view numCrew purplePearl
-- 38

-- |
-- >>> _numCrew purplePearl
-- 38

----------
-- Ship --
----------
data Ship = Ship
  { _name ∷ String,
    _numCrew ∷ Int
  }
  deriving (Show)

purplePearl ∷ Ship
purplePearl =
  Ship
    { _name = "Purple Pearl",
      _numCrew = 38
    }

makeLenses ''Ship

-- numCrew ∷ Lens' Ship Int
-- numCrew = lens _numCrew (\ship newNumCrew → ship {_numCrew = newNumCrew})

-- name ∷ Lens' Ship String
-- name = lens _name (\ship newName → ship {_name = newName})

-- |
-- >>> purplePearl
-- Ship {_name = "Purple Pearl", _numCrew = 38}

-- |
-- >>> set numCrew 41 purplePearl
-- Ship {_name = "Purple Pearl", _numCrew = 41}

-- |
-- >>> set name "More Purple Pearl" purplePearl
-- Ship {_name = "More Purple Pearl", _numCrew = 38}

-- |
-- >>> over numCrew (+3) purplePearl
-- Ship {_name = "Purple Pearl", _numCrew = 41}

-- |
-- >>> set numCrew (view numCrew purplePearl + 3) purplePearl
-- Ship {_name = "Purple Pearl", _numCrew = 41}

-- |
-- 1st lens law:
-- You get back what you set (set-get)
-- >>> let newValue = "Firefly"
-- >>> view _1 (set _1 newValue ("Star Wars", "Star Trek"))
-- "Firefly"

-- |
-- >>> let newValue = "Firefly"
-- >>> view _1 (set _1 newValue ("Star Wars", "Star Trek")) == newValue
-- True

-- |
-- 2nd lens law:
-- Setting back what you got doesn't do anything (get-set)
-- >>> let structure = ("Simpsons", "Seinfeld")
-- >>> set _1 (view _1 structure) structure
-- ("Simpsons","Seinfeld")

-- |
-- >>> let structure = ("Simpsons", "Seinfeld")
-- >>> set _1 (view _1 structure) structure == structure
-- True

-- |
-- 3rd lens law:
-- Setting twice is the same as setting once (set-set)
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
    getMsg (ReallyBadError message) = message
    getMsg (ExitCode _) = "" -- Hrmm, I guess we just return ""?
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
-- userId and userName are disjoint:
-- >>> let session = Session "USER-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"
-- >>> view userInfo session
-- ("USER-1234","Joey Tribbiani")

-- Using `lensProduct`
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

-- Using `lensProduct`
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

-------------------
-- Virtual Field --
-------------------
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

----------
-- Time --
----------
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
-- >>> let time = Time 3 10
-- >>> time
-- Time {_hours = 3, _mins = 10}

-- |
-- >>> let time = Time 3 10
-- >>> set hours 40 time
-- Time {_hours = 23, _mins = 10}

-- |
-- >>> let time = Time 3 10
-- >>> set mins (-10) time
-- Time {_hours = 3, _mins = 0}

------------------------
-- alternative lenses --
------------------------
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
-- >>> let time = Time 3 10
-- >>> time
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
-- >>> let time = Time 3 10
-- >>> over mins' (+1) (Time 23 59)
-- Time {_hours = 0, _mins = 0}

---------------
-- Promotion --
---------------
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
