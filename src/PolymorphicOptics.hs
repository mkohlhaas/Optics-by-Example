-- {-# OPTIONS_GHC -ddump-splices #-}

module PolymorphicOptics where

import Control.Lens (Field1 (_1), Field2 (_2), Lens, lens, makeLenses, over, set, view)

--------------------------------------------------------------------------------------------
--                              4. Polymorphic Optics                                     --
--------------------------------------------------------------------------------------------

--------------------------------------------
-- 4.1 Introduction to Polymorphic Optics --
--------------------------------------------

----------------------------------
-- Simple vs Polymorphic Optics --
----------------------------------

-- polymorphic lens
-- Lens s t a b

-- s: structure before action
-- t: structure after action
-- a: focus before action
-- b: focus after action

---------------------------------------------
-- 4.2 When do We Need Polymorphic Lenses? --
---------------------------------------------

---------------------------
-- Type-Changing Focuses --
---------------------------

-----------------------------------------------------
-- Changing type variables with polymorphic lenses --
-----------------------------------------------------

-- We can use polymorphic lenses to change the type of specific slots of a tuple's type,
-- but this principle generalizes to type variables in other data types as well.
data Promotion a where
  Promotion ∷
    { _item ∷ a,
      _discountPercentage ∷ Double
    } →
    Promotion a
  deriving (Show)

item ∷ Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where
    getter ∷ Promotion a → a
    getter = _item
    setter ∷ Promotion a → b → Promotion b
    setter promo newItem = promo {_item = newItem}

-- e066
e066 ∷ Promotion [String]
e066 =
  let peachPromo = Promotion "A really delicious Peach" 25.0
      buffyFigurines = ["Buffy", "Angel", "Willow", "Giles"]
   in set item buffyFigurines peachPromo

-- |
-- >>> e066
-- Promotion {_item = ["Buffy","Angel","Willow","Giles"], _discountPercentage = 25.0}

-- Can we write polymorphic Lenses for `best` and `worst`?
-- No. A Lens focuses always on one thing only.
-- You would need a Traversal.
data Preferences a where
  Preferences ∷
    { _best ∷ a,
      _worst ∷ a
    } →
    Preferences a
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

data Preferences' a b where
  Preferences' ∷
    { _best' ∷ a,
      _worst' ∷ b
    } →
    Preferences' a b
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
data Result e where
  Result ∷
    { _lineNumber ∷ Int,
      _result ∷ Either e String
    } →
    Result e

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

data ParseResult e a where
  Error ∷ e → ParseResult e a
  ParseResult ∷ a → ParseResult e a
  deriving (Show)

parseResult ∷ Lens (ParseResult e a) (ParseResult f b) (Either e a) (Either f b)
parseResult = lens getter setter
  where
    getter ∷ ParseResult a b → Either a b
    getter (Error e) = Left e
    getter (ParseResult a) = Right a
    setter ∷ p → Either e a → ParseResult e a
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

--------------------------
-- 4.3 Composing Lenses --
--------------------------

------------------------------------------------------
-- How do I Update Fields in Deeply Nested Records? --
------------------------------------------------------

-- Deeply Nested Types
data Person where
  Person ∷
    { _fullName ∷ String,
      _address ∷ Address
    } →
    Person
  deriving (Show)

data Address where
  Address ∷
    { _streetAddress ∷ StreetAddress,
      _city ∷ String,
      _country ∷ String
    } →
    Address
  deriving (Show)

data StreetAddress where
  StreetAddress ∷
    { _streetNumber ∷ String,
      _streetName ∷ String
    } →
    StreetAddress
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

e067 ∷ Person
e067 = setStreetNumber "221A" sherlock

-- |
-- >>> e067
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
e068 ∷ Person
e068 = (updateAddress . updateStreetAddress . updateStreetNumber) (const "221A") sherlock

-- |
-- >>> e068
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

data Item a where
  Item ∷
    { _material ∷ a,
      _amount ∷ Int
    } →
    Item a
  deriving (Show)

makeLenses ''Item

-- This generates the following lenses:
-- material ∷ Lens (Item a) (Item b) a b
-- amount ∷ Lens' (Item a) Int

weave ∷ Wool → Sweater
weave Wool = Sweater

gameState ∷ (Player, Item Wool)
gameState = (Player, Item Wool 5)

-- e069
e069 ∷ (Player, Item Sweater)
e069 = over (_2 . material) weave gameState

-- |
-- >>> e069
-- (Player,Item {_material = Sweater, _amount = 5})

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

-- 4. Find a way to compose ALL of the following lenses together into one big path using each exactly once. What's the type of the resulting lens?

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
