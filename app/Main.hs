{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Control.Applicative (Applicative (liftA2), ZipList (ZipList), (<|>))
import Control.Arrow ((>>>))
import Control.Lens
  ( AsEmpty (_Empty),
    At (..),
    Each (each),
    Field1 (_1),
    Field2 (_2),
    Field3 (_3),
    Fold,
    Identity (Identity),
    Index,
    Indexable (indexed),
    Indexed,
    IndexedFold,
    IndexedTraversal,
    Iso,
    Iso',
    IxValue,
    Ixed (..),
    Lens,
    Lens',
    Magnify (magnify),
    Prism,
    Prism',
    Traversal,
    Traversal',
    Zoom (zoom),
    allOf,
    anyOf,
    backwards,
    beside,
    both,
    cloneIndexPreservingLens,
    coerced,
    dimapping,
    dropping,
    droppingWhile,
    elemOf,
    element,
    elementOf,
    enum,
    failing,
    failover,
    filtered,
    filteredBy,
    findOf,
    firstOf,
    flipped,
    foldByOf,
    foldMapByOf,
    foldMapOf,
    foldOf,
    folded,
    folding,
    forOf_,
    from,
    has,
    hasn't,
    icompose,
    ifolding,
    index,
    indexing,
    indices,
    involuted,
    isn't,
    iso,
    itoListOf,
    itraverseOf_,
    itraversed,
    lastOf,
    lengthOf,
    lens,
    lined,
    makeClassy,
    makeFields,
    makeLenses,
    makePrisms,
    makeWrapped,
    mapping,
    matching,
    maximumByOf,
    maximumOf,
    minimumByOf,
    minimumOf,
    non,
    only,
    outside,
    over,
    partsOf,
    pre,
    prefixed,
    preview,
    prism,
    prism',
    productOf,
    reindexed,
    reversed,
    review,
    sans,
    selfIndex,
    sequenceAOf,
    set,
    sumOf,
    swapped,
    taking,
    takingWhile,
    to,
    toListOf,
    traverseOf,
    traverseOf_,
    traversed,
    uncurried,
    unsafePartsOf,
    use,
    uses,
    view,
    worded,
    (#),
    (%%~),
    (%@~),
    (%~),
    (&),
    (&&~),
    (*~),
    (+=),
    (+~),
    (-~),
    (.=),
    (.~),
    (//~),
    (<+~),
    (<.),
    (<.>),
    (<<+~),
    (<<>~),
    (<>=),
    (<>~),
    (<~),
    (?~),
    (^.),
    (^..),
    (^?),
    (^@..),
    (^~),
    _Cons,
    _Just,
    _Left,
    _Nothing,
    _Right,
    _Show,
    _Wrapped,
    _Wrapped',
    _Wrapping,
    _Wrapping',
    _head,
    _tail,
    (||~),
  )
import Control.Lens.Extras (biplate)
import Control.Lens.Unsound (lensProduct)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadIO (..), StateT, evalState, execState, execStateT, get, modify)
import DB
import DBClassy
import Data.Bits.Lens (bitAt)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isNumber, isSpace, isUpper, toLower, toUpper)
import Data.Coerce (Coercible, coerce)
import Data.Either.Validation (Validation (..))
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.List (elemIndex, find, intercalate, nub, sort, sortOn, stripPrefix, transpose)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Dual (Dual), Sum (Sum))
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Tree (Tree (Node))
import GHC.Word (Word8)
import Init (HasHName (..), HasPNumber (..), initialisieren)
import Numeric.Lens (adding, dividing, multiplying, negated)
import Text.Printf (printf)
import Text.Read (readMaybe)

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

--------------------------------------------------------------------------------------------
--                                      2. Optics                                         --
--------------------------------------------------------------------------------------------

---------------------------------------
-- 2.4. Practical Optics at a Glance --
---------------------------------------

-- Update portions of immutable data structures.
e001 ∷ (Char, Char, Bool)
e001 = set _3 False ('a', 'b', 'c')

-- |
-- >>> e001
-- ('a','b',False)

-- We can perform a task over deeply nested subsets of data.
-- Let's sum all numbers in a `Left` within the right half of each tuple.
e002 ∷ Integer
e002 = sumOf (folded . _2 . _Left) [(True, Left 10), (False, Right "pepporoni"), (True, Left 20)]

-- |
-- >>> e002
-- 30

-- Truncate any stories longer than 10 characters, leaving shorter ones alone.
stories, e003 ∷ [String]
stories = ["This one time at band camp", "Nuff!", "This is a short story"]
e003 = over (traversed . filtered ((> 10) . length)) (\story → take 10 story ++ " ...") stories

-- |
-- >>> e003
-- ["This one t ...","Nuff!","This is a  ..."]

----------------------------------------
-- 2.5 Impractical Optics at a Glance --
----------------------------------------

-- Summarize a list of numbers, subtracting the 'Left's, adding the 'Right's!
e004 ∷ Integer
e004 = sumOf (folded . beside negated id) [Left 1, Right 10, Left 2, Right 20]

-- |
-- >>> e004
-- 27

-- Capitalize each word in a sentence.
e005 ∷ String
e005 = "why is a raven like a writing desk" & worded . _head %~ toUpper

-- |
-- >>> e005
-- "Why Is A Raven Like A Writing Desk"

-- Multiply every Integer by 100 no matter where they are in the structure.
e006 ∷ (Maybe Int, Either (String, [Int]) Int)
e006 = (Just 3, Left ("hello", [13, 15, 17])) & biplate *~ (100 ∷ Int)

-- |
-- >>> e006
-- (Just 300,Left ("hello",[1300,1500,1700]))

-- Reverse the ordering of all even numbers in a sequence.
-- We leave the odd numbers alone!
e007 ∷ [Integer]
e007 = [1, 2, 3, 4, 5, 6, 7, 8] & partsOf (traversed . filtered even) %~ reverse

-- |
-- >>> e007
-- [1,8,3,6,5,4,7,2]

-- Sort all the characters in all strings, across word boundaries!
e008 ∷ (String, String, String)
e008 = ("one", "two", "three") & partsOf (each . traversed) %~ sort

-- |
-- >>> e008
-- ("eee","hno","orttw")

-- Flip the 2nd bit of each number to a 0
e009 ∷ [Int]
e009 = [1, 2, 3, 4] & traversed . bitAt 1 %~ not

-- |
-- >>> e009
-- [3,0,1,6]

-- Prompt the user with each question in a tuple, then return the tuple with each prompt replaced with the user's input.
prompts ∷ (String, String, String)
prompts = ("What is your name?", "What is your quest?", "What is your favourite color?")

e010 ∷ IO (String, String, String)
e010 = prompts & each %%~ (\prompt → putStrLn prompt >> getLine)

-- What is your name?
-- > Sir Galahad
-- What is your quest?
-- > To seek the holy grail
-- What is your favourite color?
-- > Blue I think?
-- ("Sir Galahad","To seek the holy grail","Blue I think?")

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
  Ship ∷
    { _shipName ∷ String,
      _numCrew ∷ Int
    } →
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
  Session ∷
    { _userId ∷ UserId,
      _userName ∷ UserName
    } →
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
  Temperature ∷
    { _location ∷ String,
      _celsius ∷ Float
    } →
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
  User ∷
    { _firstName ∷ String,
      _lastName ∷ String,
      _userEmail ∷ String
    } →
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
  Time ∷
    { _hours ∷ Int,
      _mins ∷ Int
    } →
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
  ProducePrices ∷
    { _limePrice ∷ Float,
      _lemonPrice ∷ Float
    } →
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

--------------------------------------------------------------------------------------------
--                                    5. Operators                                        --
--------------------------------------------------------------------------------------------

--------------------
-- Lens Operators --
--------------------

-- Action     | Operator | Type
-- -----------+----------+-----------------------------------
-- flip view  | ^.       | s → Lens' s a → a
-- set        | .∼       | Lens s t a b → b → s → t
-- over       | %∼       | Lens s t a b → (a → b) → s → t

----------------------
-- `view` a.k.a. ^. --
----------------------

data Payload where
  Payload ∷
    { _weightKilos ∷ Int,
      _cargo ∷ String
    } →
    Payload
  deriving (Show)

newtype Boat = Boat {_payload ∷ Payload} deriving (Show)

makeLenses ''Payload
makeLenses ''Boat

serenity ∷ Boat
serenity = Boat (Payload 50000 "Livestock")

-- e070
e070 ∷ String
e070 = view (payload . cargo) serenity

-- |
-- >>> e070
-- "Livestock"

-- `^.` is the FLIPPED version of `view`
e071 ∷ String
e071 = serenity ^. payload . cargo

-- |
-- >>> e071
-- "Livestock"

---------------------
-- `set` a.k.a. .∼ --
---------------------

e072 ∷ Boat
e072 = set (payload . cargo) "Medicine" serenity

-- |
-- >>> e072
-- Boat {_payload = Payload {_weightKilos = 50000, _cargo = "Medicine"}}

-- e073
e073 ∷ Boat
e073 = serenity & payload . cargo .~ "Medicine"

-- |
-- >>> e073
-- Boat {_payload = Payload {_weightKilos = 50000, _cargo = "Medicine"}}

------------------------------
-- Chaining Many Operations --
------------------------------

e074 ∷ Boat
e074 = serenity & set (payload . cargo) "Chocolate" & set (payload . weightKilos) 2310

-- |
-- >>> e074
-- Boat {_payload = Payload {_weightKilos = 2310, _cargo = "Chocolate"}}

-- Using operators.
e075 ∷ Boat
e075 = serenity & payload . cargo .~ "Chocolate" & payload . weightKilos .~ 2310

-- |
-- >>> e075
-- Boat {_payload = Payload {_weightKilos = 2310, _cargo = "Chocolate"}}

----------------------
-- `%∼` a.k.a. over --
----------------------

-- % is often the MOD-ulo operator, and `over` MOD-ifies its focus.

e076 ∷ Boat
e076 = serenity & payload . weightKilos %~ subtract 1000 & payload . cargo .~ "Chocolate"

-- |
-- >>> e076
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

-- e077
e077 ∷ (Integer, Integer)
e077 = (2, 30) & _2 +~ 5

-- |
-- >>> e077
-- (2,35)

-- e078
e078 ∷ (Integer, Integer)
e078 = (2, 30) & _2 -~ 5

-- |
-- >>> e078
-- (2,25)
e079 ∷ (Integer, Integer)
e079 = (2, 30) & _2 *~ 5

-- |
-- >>> e079
-- (2,150)
e080 ∷ (Integer, Double)
e080 = (2, 30) & _2 //~ 5

-- |
-- >>> e080
-- (2,6.0)
e081 ∷ (Integer, Integer)
e081 = (2, 30) & _1 ^~ 3

-- |
-- >>> e081
-- (8,30)
e082 ∷ (Bool, Integer)
e082 = (False, 30) & _1 ||~ True

-- |
-- >>> e082
-- (True,30)
e083 ∷ (Bool, Integer)
e083 = (True, 30) & _1 &&~ True

-- |
-- >>> e083
-- (True,30)
e084 ∷ (String, Integer)
e084 = ("abra", 30) & _1 <>~ "cadabra"

-- |
-- >>> e084
-- ("abracadabra",30)

---------------
-- Modifiers --
---------------

newtype Thermometer = Thermometer
  { _temperature ∷ Int
  }
  deriving (Show)

makeLenses ''Thermometer

e085 ∷ Thermometer
e085 = Thermometer 20 & temperature +~ 15

-- |
-- >>> e085
-- Thermometer {_temperature = 35}

-- Get new focus.
-- `<` = get the NEW focus in addition to modification
e086 ∷ (Int, Thermometer)
e086 = Thermometer 20 & temperature <+~ 15

-- |
-- >>> e086
-- (35,Thermometer {_temperature = 35})

-- Get old focus.
-- `<<` = get the OLD focus in addition to modification
e087 ∷ (Int, Thermometer)
e087 = Thermometer 20 & temperature <<+~ 15

-- |
-- >>> e087
-- (20,Thermometer {_temperature = 35})

---------------------------------------------
-- When to use operators vs named actions? --
---------------------------------------------

-- Author suggests to use the named versions when PARTIALLY APPLYING lens expressions, and use the operator versions the rest of the time.

e088 ∷ [String]
e088 = map (view _2) [("Patrick", "Star"), ("SpongeBob", "SquarePants")]

-- |
-- >>> e088
-- ["Star","SquarePants"]

-- Author thinks this looks a bit stupid.
e089 ∷ [String]
e089 = map (^. _2) [("Patrick", "Star"), ("SpongeBob", "SquarePants")]

-- |
-- >>> e089
-- ["Star","SquarePants"]
e090 ∷ [(String, String)]
e090 = map (over _2 reverse) [("Patrick", "Star"), ("SpongeBob", "SquarePants")]

-- |
-- >>> e090
-- [("Patrick","ratS"),("SpongeBob","stnaPerauqS")]

-- Author thinks this looks a bit stupid.
e091 ∷ [(String, String)]
e091 = map (_2 %~ reverse) [("Patrick", "Star"), ("SpongeBob", "SquarePants")]

-- |
-- >>> e091
-- [("Patrick","ratS"),("SpongeBob","stnaPerauqS")]

---------------------------
-- Exercises - Operators --
---------------------------

-- 1. Consider the following list of types:

data Gate where
  Gate ∷
    { _open ∷ Bool,
      _oilTemp ∷ Float
    } →
    Gate
  deriving (Show)

data Army where
  Army ∷
    { _archers ∷ Int,
      _knights ∷ Int
    } →
    Army
  deriving (Show)

data Kingdom where
  Kingdom ∷
    { _kname ∷ String,
      _army ∷ Army,
      _gate ∷ Gate
    } →
    Kingdom
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
--                                      6. Folds                                          --
--------------------------------------------------------------------------------------------

-------------------------------
-- 6.1 Introduction to Folds --
-------------------------------

-- Folds are like queries.

-- The key differences between lenses and folds are that:
-- 1. Lenses must focus ONE thing, Folds can focus MANY things
-- 2. Lenses can get and set, Folds can ONLY get.

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

data CrewMember where
  CrewMember ∷
    { _crewMemberName ∷ String,
      _role ∷ Role,
      _talents ∷ [String]
    } →
    CrewMember
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

-- Generic type of a fold:
--
-- Fold s a
--
-- This type says that if you give us a structure of type `s` we can find zero or more focuses of type `a`.
-- Note that a fold only specifies how to find the focuses, not how to combine them!!!
-- The decision of how to mix them all together is left up to the action.

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

-- ^ .. needs a Fold!

e092 ∷ [CrewMember]
e092 = roster ^.. crewMembers

-- |
-- >>> e092
-- [CrewMember {_crewMemberName = "Grumpy Roger", _role = Gunner, _talents = ["Juggling","Arbitrage"]},CrewMember {_crewMemberName = "Long-John Bronze", _role = PowderMonkey, _talents = ["Origami"]},CrewMember {_crewMemberName = "One-eyed Jack", _role = Navigator, _talents = []},CrewMember {_crewMemberName = "Salty Steve", _role = PowderMonkey, _talents = ["Charcuterie"]}]

-- `Maybe` is Foldable!
e093 ∷ [String]
e093 = Just "Buried Treasure" ^.. folded

-- |
-- >>> e093
-- ["Buried Treasure"]

-- `folded` might focus zero elements
e094 ∷ [a]
e094 = Nothing ^.. folded

-- |
-- >>> e094
-- []

-- e095
e095 ∷ [String]
e095 = Identity "Cutlass" ^.. folded

-- |
-- >>> e095
-- ["Cutlass"]

-- The Foldable instance of tuple only focuses the right-hand value.
e096 ∷ [String]
e096 = ("Rubies", "Gold") ^.. folded

-- |
-- >>> e096
-- ["Gold"]

-- |
-- Folding a Map focuses only the values not the keys.
e097 ∷ [String]
e097 = M.fromList [("Jack", "Captain"), ("Will", "First Mate")] ^.. folded

-- |
-- >>> e097
-- ["Captain","First Mate"]

---------------------------
-- Using Lenses as Folds --
---------------------------

-- Lenses can be used directly as folds!!!
-- You can drop in a lens anywhere you need a fold.

-- When we use a lens as a fold we can mentally substitute the types like this:
-- `Lens' s a` becomes `Fold s a`

-- e098
e098 ∷ [Role]
e098 =
  let jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]
   in jerry ^.. role

-- |
-- >>> e098
-- [PowderMonkey]

-- comparing with `view`
e099 ∷ Role
e099 =
  let jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]
   in jerry ^. role

-- |
-- >>> e099
-- PowderMonkey

---------------------
-- Composing Folds --
---------------------

e100 ∷ [Role]
e100 = roster ^.. folded . role

-- |
-- >>> e100
-- [Gunner,PowderMonkey,Navigator,PowderMonkey]
e101 ∷ [Role]
e101 =
  let jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]
   in jerry ^.. role

-- |
-- >>> e101
-- [PowderMonkey]

-----------------------------------
-- Foundational Fold Combinators --
-----------------------------------

-- `both` & `each`

-- `both` allows us to fold over BOTH parameters when the parameters are the SAME!
-- `each` allows us to fold over ALL  parameters when the parameters are the SAME!

-- `both` on tuples focuses both at once
e102 ∷ [String]
e102 = ("Gemini", "Leo") ^.. both

-- |
-- >>> e102
-- ["Gemini","Leo"]

-- compare
e103 ∷ [String]
e103 = ("Gemini", "Leo") ^.. folded

-- |
-- >>> e103
-- ["Leo"]

-- `both` on an Either type focuses whichever side is present
e104 ∷ [String]
e104 = Left "Albuquerque" ^.. both

-- |
-- >>> e104
-- ["Albuquerque"]

-- e105
e105 ∷ [String]
e105 = Right "Yosemite" ^.. both

-- |
-- >>> e105
-- ["Yosemite"]

-- Only the last two type params of a tuple are 'bitraversable'
e106 ∷ [String]
e106 = ("Gemini", "Leo", "Libra") ^.. both

-- |
-- >>> e106
-- ["Leo","Libra"]

-- if you want all members you need `each`
e107 ∷ [String]
e107 = ("Gemini", "Leo", "Libra") ^.. each

-- |
-- >>> e107
-- ["Gemini","Leo","Libra"]

-- selects each element of a list
e108 ∷ [Integer]
e108 = [1, 2, 3, 4, 5] ^.. each

-- |
-- >>> e108
-- [1,2,3,4,5]

------------------------------
-- Exercises - Simple Folds --
------------------------------

-- 1. What's the result of each expression? Make sure to guess before trying it out in the repl!

beastSizes ∷ [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

-- e109
e109 ∷ [(Int, String)]
e109 = beastSizes ^.. folded

-- |
-- >>> e109
-- [(3,"Sirens"),(882,"Kraken"),(92,"Ogopogo")]
e110 ∷ [String]
e110 = beastSizes ^.. folded . folded

-- |
-- >>> e110
-- ["Sirens","Kraken","Ogopogo"]

-- e111
e111 ∷ String
e111 = beastSizes ^.. folded . folded . folded

-- |
-- >>> e111
-- "SirensKrakenOgopogo"
e112 ∷ [String]
e112 = beastSizes ^.. folded . _2

-- |
-- >>> e112
-- ["Sirens","Kraken","Ogopogo"]

-- e113
e113 ∷ [[Integer]]
e113 = toListOf folded [[1, 2, 3], [4, 5, 6]]

-- |
-- >>> e113
-- [[1,2,3],[4,5,6]]

-- e114
e114 ∷ [Integer]
e114 = toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]

-- |
-- >>> e114
-- [1,2,3,4,5,6]

-- e115
e115 ∷ String
e115 = toListOf (folded . folded) (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])

-- |
-- >>> e115
-- "CaptainFirst Mate"

-- e116
e116 ∷ String
e116 = ("Hello", "It's me") ^.. both . folded

-- |
-- >>> e116
-- "HelloIt's me"

-- e117
e117 ∷ [String]
e117 = ("Why", "So", "Serious?") ^.. each

-- |
-- >>> e117
-- ["Why","So","Serious?"]
quotes ∷ [(String, String, String)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

-- e118
e118 ∷ [(String, String, String)]
e118 = quotes ^.. each

-- |
-- >>> e118
-- [("Why","So","Serious?"),("This","is","SPARTA")]
e119 ∷ [String]
e119 = quotes ^.. each . each

-- |
-- >>> e119
-- ["Why","So","Serious?","This","is","SPARTA"]
e120 ∷ String
e120 = quotes ^.. each . each . each

-- |
-- >>> e120
-- "WhySoSerious?ThisisSPARTA"

-- 2. Write out the specialized type for each of the requested combinators used in each of the following expressions.

-- a) folded, _1, toListOf

-- e121
e121 ∷ [Integer]
e121 = toListOf (folded . _1) [(1, 'a'), (2, 'b'), (3, 'c')]

-- |
-- >>> e121
-- [1,2,3]

-- folded ∷ Fold [(Int, Char)] (Int, Char)
-- _1 ∷ Fold (Int, Char) Int
-- toListOf ∷ Fold [(Int, Char)] → [(Int, Char)] → [Int]

-- b) _2, folded, toListOf

-- e122
e122 ∷ [String]
e122 = toListOf (_2 . folded) (False, S.fromList ["one", "two", "three"])

-- |
-- >>> e122
-- ["one","three","two"]

-- _2 ∷ Fold (Bool, Set String) (Set String)
-- folded ∷ Fold (Set String) String
-- toListOf ∷ Fold (Bool, Set String) String → (Bool, Set String) → [String]

-- c) folded, folded, toListOf

-- e123
e123 ∷ String
e123 = toListOf (folded . folded) (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])

-- |
-- >>> e123
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

-- e124
e124 ∷ [Integer]
e124 = [1, 2, 3] ^.. folded

-- |
-- >>> e124
-- [1,2,3]

-- e125
e125 ∷ [String]
e125 = ("Light", "Dark") ^.. _1

-- |
-- >>> e125
-- ["Light"]

-- e126
e126 ∷ [String]
e126 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . both

-- |
-- >>> e126
-- ["Light","Dark","Happy","Sad"]

-- e127
e127 ∷ [String]
e127 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . each

-- |
-- >>> e127
-- ["Light","Dark","Happy","Sad"]

-- e128
e128 ∷ [String]
e128 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1

-- |
-- >>> e128
-- ["Light","Happy"]

-- e129
e129 ∷ String
e129 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . folded . folded

-- |
-- >>> e129
-- "DarkSad"
e130 ∷ [String]
e130 = ("Bond", "James", "Bond") ^.. each

-- |
-- >>> e130
-- ["Bond","James","Bond"]

----------------------
-- 6.2 Custom Folds --
----------------------

newtype Name = Name {getName ∷ String}
  deriving (Show)

-- Not foldable, not even having a type parameter.
data ShipCrew where
  ShipCrew ∷
    { _ship ∷ Name,
      _captain ∷ Name,
      _firstMate ∷ Name,
      _conscripts ∷ [Name]
    } →
    ShipCrew
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

-- e131
e131 ∷ [Name]
e131 = myCrew ^.. allCrewMembers

-- |
-- >>> e131
-- [Name {getName = "Grumpy Roger"},Name {getName = "Long-John Bronze"},Name {getName = "One-eyed Jack"},Name {getName = "Filthy Frank"}]

------------------------
-- Mapping over Folds --
------------------------

-- The `to` helper is pretty simple, it converts a function directly into a fold!
-- to ∷ (s → a) → Fold s a
-- It's like mapping over the Fold.

-- `to` allows us to easily interleave function transformations into a path of composed optics
e132 ∷ [String]
e132 = myCrew ^.. allCrewMembers . to getName

-- |
-- >>> e132
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]

-- e133
e133 ∷ String
e133 = Name "Two-faced Tony" ^. to getName

-- |
-- >>> e133
-- "Two-faced Tony"

-- We can chain many `to`s in a row
e134 ∷ String
e134 = Name "Two-faced Tony" ^. to getName . to (fmap toUpper)

-- |
-- >>> e134
-- "TWO-FACED TONY"

-- e135
e135 ∷ String
e135 = Name "Two-faced Tony" ^. to (fmap toUpper . getName)

-- |
-- >>> e135
-- "TWO-FACED TONY"

-- reading from left to right throughout
e136 ∷ String
e136 = Name "Two-faced Tony" ^. to (getName >>> fmap toUpper)

-- |
-- >>> e136
-- "TWO-FACED TONY"

----------------------------------------------------
-- Combining Multiple Folds on the Same Structure --
----------------------------------------------------

-- Every lens is a valid fold!
crewNames ∷ Fold ShipCrew Name
crewNames =
  folding
    ( \s →
        s ^.. captain
          <> s ^.. firstMate
          <> s ^.. conscripts . folded
    )

-- |
-- compare `^.` and `^..`
-- >>> myCrew ^. captain
-- Name {getName = "Grumpy Roger"}

-- |
-- >>> myCrew ^.. captain
-- [Name {getName = "Grumpy Roger"}]

-- |
-- >>> myCrew ^.. firstMate
-- [Name {getName = "Long-John Bronze"}]

-- |
-- >>> myCrew ^.. conscripts . folded
-- [Name {getName = "One-eyed Jack"},Name {getName = "Filthy Frank"}]

-- |
-- >>> myCrew ^.. crewNames
-- [Name {getName = "Grumpy Roger"},Name {getName = "Long-John Bronze"},Name {getName = "One-eyed Jack"},Name {getName = "Filthy Frank"}]

-- |
-- >>> myCrew ^.. crewNames . to getName
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]
e137 ∷ [String]
e137 = myCrew ^.. crewNames . to getName

-- |
-- >>> e137
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]

------------------------------
-- Exercises - Custom Folds --
------------------------------

-- 1. Fill in each blank with either `to`, `folded`, or `folding`.

-- >>> ["Yer", "a", "wizard", "Harry"] ^.. folded . _
-- "YerawizardHarry"

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . _
-- [1,2,3,4,5,6]

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
-- [1, 2, 4, 5]

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
-- [[1,2], [4,5]]

-- >>> ["bob", "otto", "hannah"] ^.. folded . _ reverse
-- ["bob", "otto", "hannah"]

-- >>> ("abc", "def") ^.. _ (\(a, b) → [a, b]) . _ reverse . _
-- "cbafed"

-- Solutions:

-- |
-- >>> ["Yer", "a", "wizard", "Harry"] ^.. folded . folded
-- "YerawizardHarry"

-- |
-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . folded
-- [1,2,3,4,5,6]

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

-- The blanks have been all at the end of the expression, e.g.  "[1..5] ^.. _"

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

-- >>> [(1, (2, 3)), (4, (5, 6))] ^.. folded . to (\(x, (y, z)) → [x, y, z]) . folded
-- [1,2,3,4,5,6]

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

-- Notice the pattern!!!
-- to fn . folded == folding fn

-- 3. BONUS - Devise a fold which returns the expected results. Think outside the box a bit.

-- |
-- >>> [(12, 45, 66), (91, 123, 87)] ^.. folded . _2 . to show . to reverse . folded
-- "54321"

-- |
-- >>> [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . folding (\(x , y) → if even x then Just y else Nothing)
-- ["b","d"]

-- |
-- >>> [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . folding (\(x, y) → if even x then [y] else [])
-- ["b","d"]

----------------------
-- 6.3 Fold Actions --
----------------------

--------------------------------
-- Writing Queries with Folds --
--------------------------------

-- A rule of thumb when looking for which action to use on a fold: think of the function you'd use
-- on normal ol’ Haskell list for the same purpose, then just add the suffix -Of!
-- sum → sumOf
-- minimum → minimumOf

-- sumOf ∷ Num a ⇒ Getting (Endo (Endo a)) s a → s → a

-- It's really not important to actually know what a Getting (Endo (Endo a)) is; I don't
-- personally know the actual types of most of these actions. What I DO know though is that when
-- I see a Getting (Some Crazy Type) s a I know I can substitute nearly any optic into that slot,
-- including a (Fold s a) or a (Lens' s a). The “behind the scenes” types can unify themselves with
-- the (Some Crazy Type) portion of a Getter; so usually I mentally just substitute any Getting _ s a
-- with a Fold s a.

-- >>> :type sum
-- sum ∷ (Foldable t, Num a) ⇒ t a → a

-- >>> :type sumOf
-- sumOf ∷ Num a ⇒ Getting (Endo (Endo a)) s a → s → a

-- |
-- >>> sum [1, 2, 3, 4]
-- 10

-- |
-- >>> sumOf folded [1, 2, 3, 4]
-- 10

-- |
-- >>> elem 3 [1, 2, 3, 4]
-- True

-- |
-- Does my fold contain a given element?
-- >>> elemOf folded 3 [1, 2, 3, 4]
-- True

-- |
-- >>> elem 99 [1, 2, 3, 4]
-- False

-- |
-- >>> elemOf folded 99 [1, 2, 3, 4]
-- False

-- |
-- >>> any even [1, 2, 3, 4]
-- True

-- |
-- Do ANY focuses match a predicate?
-- >>> anyOf folded even [1, 2, 3, 4]
-- True

-- |
-- >>> any (> 10) [1, 2, 3, 4]
-- False

-- |
-- >>> anyOf folded (> 10) [1, 2, 3, 4]
-- False

-- |
-- >>> all even [1, 2, 3, 4]
-- False

-- |
-- Do ALL focuses match a predicate?
-- >>> allOf folded even [1, 2, 3, 4]
-- False

-- >>> all (< 10) [1, 2, 3, 4]
-- True

-- |
-- >>> allOf folded (< 10) [1, 2, 3, 4]
-- True

-- |
-- >>> find even [1, 2, 3, 4]
-- Just 2

-- |
-- Find the first element matching a predicate
-- >>> findOf folded even [1, 2, 3, 4]
-- Just 2

-- |
-- >>> find (> 10) [1, 2, 3, 4]
-- Nothing

-- |
-- >>> findOf folded (> 10) [1, 2, 3, 4]
-- Nothing

-- |
-- >>> not . null $ []
-- False

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
-- >>> length [1, 2, 3, 4]
-- 4

-- |
-- How many focuses are there?
-- >>> lengthOf folded [1, 2, 3, 4]
-- 4

-- >>> product [1, 2, 3, 4]
-- 24

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
-- >>> minimum [2, 1, 4, 3]
-- 1

-- |
-- Find the minimum focus
-- >>> minimumOf folded [2, 1, 4, 3]
-- Just 1

-- |
-- >>> minimumOf folded []
-- Nothing

-- |
-- >>> maximum [2, 1, 4, 3]
-- 4

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

data Actor where
  Actor ∷ {_actorName ∷ String, _birthYear ∷ Int} → Actor
  deriving (Show, Eq)

makeLenses ''Actor

data TVShow where
  TVShow ∷
    { _title ∷ String,
      _numEpisodes ∷ Int,
      _numSeasons ∷ Int,
      _criticScore ∷ Double,
      _actors ∷ [Actor]
    } →
    TVShow
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
-- >>> tvShows ^.. folded . actors . folded
-- [Actor {_actorName = "Josh Radnor", _birthYear = 1974},Actor {_actorName = "Cobie Smulders", _birthYear = 1982},Actor {_actorName = "Neil Patrick Harris", _birthYear = 1973},Actor {_actorName = "Alyson Hannigan", _birthYear = 1974},Actor {_actorName = "Jason Segel", _birthYear = 1980},Actor {_actorName = "Sarah Michelle Gellar", _birthYear = 1977},Actor {_actorName = "Alyson Hannigan", _birthYear = 1974},Actor {_actorName = "Nicholas Brendon", _birthYear = 1971},Actor {_actorName = "David Boreanaz", _birthYear = 1969},Actor {_actorName = "Anthony Head", _birthYear = 1954}]

-- |
-- >>> lengthOf (folded . actors . folded) tvShows
-- 10

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
-- >>> tvShows ^.. folded . actors . folded . to showActor
-- ["Josh Radnor: 56","Cobie Smulders: 48","Neil Patrick Harris: 57","Alyson Hannigan: 56","Jason Segel: 50","Sarah Michelle Gellar: 53","Alyson Hannigan: 56","Nicholas Brendon: 59","David Boreanaz: 61","Anthony Head: 76"]
e138 ∷ IO ()
e138 = traverseOf_ (folded . actors . folded . to showActor) putStrLn tvShows

-- has the form: a → m ()
-- >>> :type putStrLn
--              a    → m  ()
--              |      |
-- putStrLn ∷ String → IO ()

-- |
-- >>> e138
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

-- has the form: a → m ()
-- >>> :type modify
--                              a    → m ()
--                              |      |
-- modify ∷ MonadState s m ⇒ (s → s) → m ()

-- Oftentimes it's handy to build up a stateful computation from the focuses of a fold.
e139 ∷ Integer
-- e139 = execState (traverseOf_ folded (modify . const (+ 1)) tvShows) 0
-- e139 = traverseOf_ folded (\ tvShow → modify (+ 1)) tvShows ∷ StateT Integer Identity ()
-- e139 = traverseOf_ folded (\ _ → modify (+ 1)) tvShows ∷ StateT Integer Identity ()
-- e139 = traverseOf_ folded (const $ modify (+ 1)) tvShows ∷ StateT Integer Identity ()
e139 = execState (traverseOf_ folded (modify . const (+ 1)) tvShows) 0

-- >>> :type const 1
-- const 1 ∷ b → Integer

-- >>> :type const (+ 1)
-- const (+ 1) ∷ b → Integer → Integer

-- basically:
-- const (+ 1) ∷ b → Integer → Integer
-- `b` applied to TVShow
-- const (+ 1) ∷ TVShow → Integer → Integer
-- give it a TVShow and you get a function that is expected by `modify`:
-- const (+ 1) tvShow ∷ Integer → Integer

-- |
-- >>> e139
-- 2

-- the same (only argument order changed)
e140 ∷ Integer
e140 = execState (forOf_ folded tvShows (modify . const (+ 1))) 0

-- |
-- >>> e140
-- 2

----------------------------
-- Combining Fold Results --
----------------------------

-- foldOf    ∷ Monoid a ⇒ Fold s a           → s → a
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
-- Using `view` on Folds --
---------------------------

-- A very common mistake when people get started with folds is to use `view` (^.) on a Fold instead of a Lens.
-- This is especially confusing because it actually works in some cases but not others.

-- This situation is actually caused by a bit of Lens's implementation "leaking" out.
-- The implementation of `view` accepts any type of optic, but adds constraints which usually only work on a Lens.
-- HOWEVER, if the focus you're viewing happens to be a Monoid it can "view" through the Fold by using the Monoid typeclass.

-- In general you should avoid this "weird" behaviour and just use `foldOf` explicitly when you want this behaviour!!!

-- This works just fine.
e141 ∷ String
e141 = Just "do it" ^. folded

-- |
-- >>> e141
-- "do it"

-- This one crashes and burns!
-- Compiler Error: No instance for (Monoid Int) arising from a use of ‘folded’
-- e142 = Just (42 ∷ Int) ^. folded

-- When there's a single focus, we just return it.
e142 ∷ String
e142 = Just "do it" ^. folded

-- |
-- >>> e142
-- "do it"

-- When there aren't any focuses, return 'mempty'
e143 ∷ String
e143 = (Nothing ∷ Maybe String) ^. folded

-- |
-- >>> e143
-- ""

-- When there are multiple focuses, combine them with (<>).
e144 ∷ String
e144 = ("one", "two", "three") ^. each

-- |
-- >>> e144
-- "onetwothree"

--------------------------------
-- Customizing Monoidal Folds --
--------------------------------

-- Lots of mapping folds to choose from.
-- `foldByOf`, `foldMapByOf`, `foldrOf`, `foldlOf`, ...

-- e145
e145 ∷ Map String Integer
e145 = foldMapOf (folded . actors . folded . actorName) (`M.singleton` 1) tvShows

-- |
-- >>> e145
-- fromList [("Alyson Hannigan",1),("Anthony Head",1),("Cobie Smulders",1),("David Boreanaz",1),("Jason Segel",1),("Josh Radnor",1),("Neil Patrick Harris",1),("Nicholas Brendon",1),("Sarah Michelle Gellar",1)]

-- |
-- When we combine two maps with the same keys it simply ignores duplicate values.
-- >>> M.singleton "an actor" 1 <> M.singleton "an actor" 2
-- fromList [("an actor",1)]

-- |
-- This looks better:
e146 ∷ Map String Integer
e146 = M.unionWith (+) (M.singleton "an actor" 1) (M.singleton "an actor" 2)

-- |
-- >>> e146
-- fromList [("an actor",3)]

-- |
-- "Alyson Hannigan" is in both shows.
e147 ∷ Map String Integer
e147 = foldMapByOf (folded . actors . folded . actorName) (M.unionWith (+)) mempty (`M.singleton` 1) tvShows

-- |
-- >>> e147
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

-- >>> has folded []
-- False

-- >>> foldOf both ("Yo", "Adrian!")
-- "YoAdrian!"

-- >>> elemOf each "phone" ("E.T.", "phone", "home")
-- True

-- >>> minimumOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 2

-- >>> lastOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 11

-- >>> anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
-- True

-- >>> findOf folded even [11, 22, 3, 5, 6]
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

----------------------------
-- 6.4 Higher Order Folds --
----------------------------

-- These sorts of combinators are higher-order, because usually they accept an optic as an argument and return a new one as a result.

----------------------
-- Taking, Dropping --
----------------------

-- There are many more signatures but here are the ones for Fold.
-- taking   ∷ Int → Fold s a → Fold s a
-- dropping ∷ Int → Fold s a → Fold s a

-- e.g.  `(taking 3)` accepts a Fold which focuses the first three focuses.

-- e148
e148 ∷ [Integer]
e148 = [1, 2, 3, 4] ^.. taking 2 folded

-- |
-- >>> e148
-- [1,2]

-- e149
e149 ∷ [Integer]
e149 = [1, 2, 3, 4] ^.. dropping 2 folded

-- |
-- >>> e149
-- [3,4]

-- e150
e150 ∷ [Integer]
e150 = [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded . taking 2 folded

-- | Using `to`
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded . to (take 2) . folded
-- [1,2,10,20,100,200]

-- |
-- >>> e150
-- [1,2,10,20,100,200]

-- |
-- >>> ("Albus", "Dumbledore") ^.. both . taking 3 folded
-- "AlbDum"

-- >>> ("Albus", "Dumbledore") ^.. both . to (take 3) . folded
-- "AlbDum"

-- |
-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. taking 2 (folded . folded)
-- [1,2]

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

-- e151
e151 ∷ [Integer]
e151 = [1, 2, 3] ^.. backwards folded

-- |
-- >>> e151
-- [3,2,1]

-- e152
e152 ∷ [String]
e152 = ("one", "two") ^.. backwards both

-- |
-- >>> e152
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

-- Two more higher-order folds.
-- takingWhile   ∷ (a → Bool) → Fold s a → Fold s a
-- droppingWhile ∷ (a → Bool) → Fold s a → Fold s a

-- e153
e153 ∷ [Integer]
e153 = [1, 5, 15, 5, 1] ^.. takingWhile (< 10) folded

-- |
-- >>> e153
-- [1,5]

-- |
-- >>> [1..100] ^.. takingWhile (< 10) folded
-- [1,2,3,4,5,6,7,8,9]

-- |
-- >>> [1..] ^.. takingWhile (< 10) folded
-- [1,2,3,4,5,6,7,8,9]

-- e154
e154 ∷ [Integer]
e154 = [1 .. 100] ^.. droppingWhile (< 90) folded

-- |
-- >>> e154
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

-- 1. Fill in the blank. You'll need to remember some tricks from previous sections!

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

-- >>> "blink182 k9 blazeit420" ^.. folded . filtered isNumber
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

-------------------------
-- 6.5 Filtering Folds --
-------------------------

--------------
-- Filtered --
--------------

-- `filtered` doesn't change the type of the elements as they pass through, it simply drops elements from the fold if they don't match the predicate.
-- The power comes from using `filtered` in the midst of other folds!!!

-- Its type.
-- filtered ∷ (s → Bool) → Fold s s

-- |
-- >>> [1, 2, 3, 4] ^.. folded . filtered even
-- [2,4]

-- |
-- >>> ["apple", "passionfruit", "orange", "pomegranate"] ^.. folded . filtered (length >>> (> 6))
-- ["passionfruit","pomegranate"]
data Card where
  Card ∷
    { _cardName ∷ String,
      _aura ∷ Aura,
      _holo ∷ Bool,
      _moves ∷ [Move]
    } →
    Card
  deriving (Show, Eq)

-- Each card has an aura-type
data Aura
  = Wet
  | Hot
  | Spark
  | Leafy
  deriving (Show, Eq)

-- Cards have attack moves
data Move where
  Move ∷ {_moveName ∷ String, _movePower ∷ Int} → Move
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
-- >>> lengthOf (folded . moves . folded) deck
-- 12

-- |
-- >>> deck ^.. folded . moves . folded . movePower
-- [20,20,20,10,30,50,40,3,30,40,40,50]

-- We can even use other fold queries INSIDE our filter!
-- This lets us filter elements based on a query over the currently focused piece of state!
-- List all cards which have ANY move with an attack power greater than 40.
e155 ∷ [String]
e155 = deck ^.. folded . filtered (anyOf (moves . folded . movePower) (> 40)) . cardName

-- |
-- >>> e155
-- ["Elecdude","Sparkeon"]

-- |
-- We can filter based on certain properties and then continue diving deeper into a different portion of the data.
-- How many moves do Spark cards have in total?
-- >>> lengthOf (folded . filtered ( _aura >>> (== Spark)) . moves . folded ) deck
-- 5

-- |
-- List all Spark Moves with a power greater than 30.
-- >>> deck ^.. folded . filtered ( _aura >>> (== Spark)) . moves . folded . filtered ( _movePower >>> (> 30)) . moveName
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

-- |
-- >>> deck ^.. (folded . filteredBy (holo . only True))
-- [Card {_cardName = "Garydose", _aura = Wet, _holo = True, _moves = [Move {_moveName = "Gary's move", _movePower = 40}]},Card {_cardName = "Sparkeon", _aura = Spark, _holo = True, _moves = [Move {_moveName = "Shock", _movePower = 40},Move {_moveName = "Battery", _movePower = 50}]}]

-- |
-- more idiomatic; staying in the optics world
-- >>> maximumByOf (folded . filteredBy (holo . only True)) (comparing (lengthOf moves)) deck
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
-- >>> firstOf (folded . filtered ((> 1) . length . _moves) . cardName) deck
-- Just "Kapichu"

-- |
-- Are there any Hot cards with a move with more than 30 attack power?
-- >>> anyOf (folded . filteredBy (aura . only Hot) . moves . folded . movePower) (> 30) deck
-- True

-- |
-- Are there any Hot cards with a move with more than 40 attack power?
-- >>> anyOf (folded . filteredBy (aura . only Hot) . moves . folded . movePower) (> 40) deck
-- False

-- |
-- List the names of all holographic cards with a Wet aura.
-- >>> deck ^.. folded . filtered _holo . filteredBy (aura . only Wet) .cardName
-- ["Garydose"]

-- |
-- What's the sum of all attack power for all moves belonging to non-Leafy cards?
-- >>> sumOf (folded . filtered ((/= Leafy) . _aura) . moves . folded . movePower) deck
-- 303

-------------------
-- 6.6 Fold Laws --
-------------------

-- There aren't any!

--------------------------------------------------------------------------------------------
--                                    7. Traversals                                       --
--------------------------------------------------------------------------------------------

------------------------------------
-- 7.1 Introduction to Traversals --
------------------------------------

-- Traversals are essentially what you get if you combine all the powers of folds and lenses together.
-- Lenses can get and set only a single value, whereas folds can get multiple values, but can't set or update.
-- Traversals are the fusion of the two and allow us to get or set zero or more values.

------------------------------------------------
-- How do Traversals Fit into the Hierarchy ? --
------------------------------------------------

--            |  Get   | Set/Modify | Traverse
-----------------------------------------------
-- Lens       | Single |   Single   | Single
-- Fold       |  Many  |     ✗      |   ✗
-- Traversal  |  Many  |   Many     | Many

-- A Lens can be used as anything.
-- A Fold can only be used as a Fold.
-- A Traversal can be used as a Fold but not vice versa.
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

-- Modify the focuses using the `over` action: (%∼).
e156 ∷ (String, String)
e156 = ("Bubbles", "Buttercup") & both %~ (++ "!")

-- |
-- >>> e156
-- ("Bubbles!","Buttercup!")

-- Using `set` (.∼) on a Traversal.
e157 ∷ (String, String)
e157 = ("Bubbles", "Buttercup") & both .~ "Blossom"

-- |
-- >>> e157
-- ("Blossom","Blossom")

-- Changing type of tuple (polymorphic Traversal).
e158 ∷ (Int, Int)
e158 = ("Bubbles", "Buttercup") & both %~ length

-- |
-- >>> e158
-- (7,9)

-- `each` is also a Traversal.
e159 ∷ (Integer, Integer, Integer)
e159 = (1, 2, 3) & each %~ (* 10)

-- |
-- >>> e159
-- (10,20,30)

-- e160
e160 ∷ String
e160 = "Here's Johnny" & each %~ toUpper

-- |
-- >>> e160
-- "HERE'S JOHNNY"

-- e161
e161 ∷ [Int]
e161 = "Houston we have a problem" & each .~ (22 ∷ Int)

-- |
-- >>> e161
-- [22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22]

-- e162
e162 ∷ [Integer]
e162 = [1, 2, 3, 4, 5] & taking 3 traversed *~ 10

-- |
-- >>> e162
-- [10,20,30,4,5]

-- e163
e163 ∷ [Integer]
e163 = [1, 2, 3, 4, 5] & dropping 3 traversed *~ 10

-- |
-- >>> e163
-- [1,2,3,40,50]

-- e164
e164 ∷ String
e164 = "once upon a time - optics became mainstream" & takingWhile (/= '-') traversed %~ toUpper

-- |
-- >>> e164
-- "ONCE UPON A TIME - optics became mainstream"

-- Multiply all even numbers by 10.
e165 ∷ [Integer]
e165 = [1, 2, 3, 4, 5] & traversed . filtered even *~ 10

-- |
-- >>> e165
-- [1,20,3,40,5]

-- `filtered` is an extremely powerful tool, it alters the exact same elements which would be focused when you use it in a fold.
-- Reverse only the long strings.
e166 ∷ (String, String)
e166 = ("short", "really long") & both . filtered ((> 5) . length) %~ reverse

-- |
-- >>> e166
-- ("short","gnol yllaer")

-------------------------------
-- 7.2 Traversal Combinators --
-------------------------------

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
-- >>> [1, 2, 3] ^.. traversed
-- [1,2,3]

-- |
-- Lists are traversable.
-- >>> [1, 2, 3] & traversed *~ 10
-- [10,20,30]

-- Tuples are traversable (over their LAST slot).
e167 ∷ (String, String)
e167 = ("Batman", "Superman") & traversed %~ take 3

-- |
-- >>> e167
-- ("Batman","Sup")

-- Maps are traversable.
e168 ∷ Map String String
e168 =
  let powerLevels = M.fromList [("Gohan", 710), ("Goku", 9001), ("Krillin", 5000), ("Piccolo", 408)]
   in powerLevels & traversed %~ \n → if n > 9000 then "Over 9000" else show n

-- |
-- >>> e168
-- fromList [("Gohan","710"),("Goku","Over 9000"),("Krillin","5000"),("Piccolo","408")]

-- |
-- Sets are NOT traversable.
-- let powerLevels = S.fromList [("Gohan", 710) , ("Goku", 9001) , ("Krillin", 5000) , ("Piccolo", 408)]
-- powerLevels & traversed %~ \n → if n > 9000 then "Over 9000" else show n
-- No instance for (Traversable Set) arising from a use of ‘traversed’

-- Trees are traversable.
e169 ∷ Tree String
e169 =
  let opticsTree = Node "Lens" [Node "Fold" [], Node "Traversal" []]
   in opticsTree & traversed %~ reverse

-- |
-- >>> e169
-- Node {rootLabel = "sneL", subForest = [Node {rootLabel = "dloF", subForest = []},Node {rootLabel = "lasrevarT", subForest = []}]}

----------------------
-- More Combinators --
----------------------

-- As Folds `worded` works pretty much the same as `words` from the Prelude.
e170 ∷ [String]
e170 = "I'll be back!" ^.. worded

-- |
-- >>> e170
-- ["I'll","be","back!"]

-- The same for `lined`.
e171 ∷ [String]
e171 = "Run\nForrest\nRun" ^.. lined

-- |
-- >>> e171
-- ["Run","Forrest","Run"]

-- As Traversal we can also update.
-- Surround each word with '*'s.
e172 ∷ String
e172 = "blue suede shoes" & worded %~ \s → "*" ++ s ++ "*"

-- |
-- >>> e172
-- "*blue* *suede* *shoes*"

-- Capitalize each word.
e173 ∷ String
e173 = "blue suede shoes" & worded %~ \(x : xs) → toUpper x : xs

-- |
-- >>> e173
-- "Blue Suede Shoes"

-- Add a "#" to the start of each line.
e174 ∷ String
e174 = "blue\nsuede\nshoes" & lined %~ ('#' :)

-- |
-- >>> e174
-- "#blue\n#suede\n#shoes"

-- Mapping the identity function still has the white-space collapsing side-effects of `unwords`. Newlines are getting lost.
-- Makes worded and lined unlawful but still useful.
e175 ∷ String
e175 = "blue \n suede \n \n shoes" & worded %~ id

-- >>> "blue \n suede \n \n shoes" ^.. worded
-- ["blue","suede","shoes"]

-- |
-- >>> e175
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

-- Note: `id` is a valid Optic.
e176 ∷ [String]
e176 =
  let dinos = ("T-Rex", (42, "Stegosaurus"))
   in dinos ^.. beside id _2

-- |
-- >>> e176
-- ["T-Rex","Stegosaurus"]

-- We can provide a different path to focus Ints for each half of the tuple.
e177 ∷ [Integer]
e177 =
  let numbers = ([(1, 2), (3, 4)], [5, 6, 7])
   in numbers ^.. beside (traversed . both) traversed

-- |
-- >>> e177
-- [1,2,3,4,5,6,7]

-- e178
e178 ∷ [String]
e178 = ("T-Rex", ("Ankylosaurus", "Stegosaurus")) ^.. beside id both

-- |
-- >>> e178
-- ["T-Rex","Ankylosaurus","Stegosaurus"]

-- We can modify all characters inside both halves of the tuple.
-- Each half of the tuple has a different path to focus the characters.
e179 ∷ (String, [String])
e179 = ("Cowabunga", ["let's", "order", "pizza"]) & beside traversed (traversed . traversed) %~ toUpper

-- |
-- >>> e179
-- ("COWABUNGA",["LET'S","ORDER","PIZZA"])

-- Every `Bitraversable` - not only tuples - can be used with `beside`!!!
e180 ∷ Either (Int, Int) [Int]
e180 = Left (1, 2) & beside both traversed %~ negate

-- |
-- >>> e180
-- Left (-1,-2)
--
-- Left (-1,-2)

-- e181
e181 ∷ Either (Int, Int) [Int]
e181 = Right [3, 4] & beside both traversed %~ negate

-- |
-- >>> e181
-- Right [-3,-4]

-------------------------------------------
-- Focusing a Specific Traversal Element --
-------------------------------------------

-- `element` is simple traversal which will focuses only the nth element of a Traversable container.
-- Because it doesn't focus every element of the container it's a Monomorphic Traversal.
-- element ∷ Traversable f ⇒ Int → Traversal' (f a) a

-- e182
e182 ∷ Maybe Integer
e182 = [0, 1, 2, 3, 4] ^? element 2

-- |
-- >>> e182
-- Just 2

-- `element` can't change the container's element type.
-- It is a `Monomorphic Traversal`.
e183 ∷ [Integer]
e183 = [0, 1, 2, 3, 4] & element 2 *~ 100

-- |
-- >>> e183
-- [0,1,200,3,4]

-- `elementOf` is a higher-order traversal which allows to select a specific element from an arbitrary traversal.
-- elementOf ∷ Traversal' s a → Int → Traversal' s a
-- elementOf ∷ Fold s a       → Int → Fold s a

-- `element` is basically `elementOf traversed`
e184 ∷ Maybe Integer
e184 = [0, 1, 2, 3, 4] ^? elementOf traversed 2

-- |
-- >>> e184
-- Just 2

-- We can get a specific element from a composition of traversals.
e185 ∷ Maybe Integer
e185 = [[0, 1, 2], [3, 4], [5, 6, 7, 8]] ^? elementOf (traversed . traversed) 6

-- |
-- >>> e185
-- Just 6

-- e186
e186 ∷ [[Integer]]
e186 = [[0, 1, 2], [3, 4], [5, 6, 7, 8]] & elementOf (traversed . traversed) 6 *~ 100

-- |
-- >>> e186
-- [[0,1,2],[3,4],[5,600,7,8]]

-------------------------------
-- 7.3 Traversal Composition --
-------------------------------

-- Each traversal selects focuses, then rebuilds the structure around the transformed results!!!

-- Capitalize the first char of every word.
e187 ∷ String
e187 = "blue suede shoes" & worded . taking 1 traversed %~ toUpper

-- |
-- >>> e187
-- "Blue Suede Shoes"

-- Find all strings longer than 5 chars then surround each word in that string with '*'.
e188 ∷ [String]
e188 = ["short", "really long"] & traversed . filtered ((> 5) . length) . worded %~ \s → "*" ++ s ++ "*"

-- |
-- >>> e188
-- ["short","*really* *long*"]

-- Add "Richy " to the names of people with more than $1000.
e189 ∷ ((String, Integer), (String, Integer), (String, Integer))
e189 = (("Ritchie", 100000), ("Archie", 32), ("Reggie", 4350)) & each . filtered ((> 1000) . snd) . _1 %~ ("Richy " ++)

-- |
-- >>> e189
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
-- >>> (1, (2, [3, 4])) & beside id (beside id traversed) +~ 1
-- (2,(3,[4,5]))

-- |
-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each . filteredBy (_1 . only True) . _2 . taking 5 traversed %~ toUpper
-- ((True,"STRAWberries"),(False,"Blueberries"),(True,"BLACKberries"))

-- |
-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each . filtered fst . _2 . taking 5 traversed %~ toUpper
-- ((True,"STRAWberries"),(False,"Blueberries"),(True,"BLACKberries"))

-- |
-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each %~ snd
-- ("Strawberries","Blueberries","Blackberries")

---------------------------
-- 7.4 Traversal Actions --
---------------------------

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

-- e190
e190 ∷ Maybe [Integer]
e190 = sequenceA [Just 1, Just 2, Just 3]

-- |
-- >>> e190
-- Just [1,2,3]

-- e191
e191 ∷ Maybe [Integer]
e191 = sequenceA [Just 1, Nothing, Just 3]

-- |
-- >>> e191
-- Nothing

-- sequenceA ∷ Maybe (Either String a) → Either String (Maybe a)
e192 ∷ Either String (Maybe a)
e192 = sequenceA $ Just (Left "Whoops")

-- |
-- >>> e192
-- Left "Whoops"

-- sequenceA ∷ Maybe (Either String a) → Either String (Maybe a)
e193 ∷ Either a (Maybe String)
e193 = sequenceA $ Just (Right "Whoops")

-- |
-- >>> e193
-- Right (Just "Whoops")

-- :t readMaybe
-- readMaybe ∷ Read a ⇒ String → Maybe a
-- 'readMaybe' is polymorphic so we need to specify a concrete result type.

-- traverse ∷ (Traversable t, Applicative f) ⇒ (a → f b) → t a → f (t b)

-- The Traversable is '[ ]', and the effect is 'IO'
-- (FilePath → IO String) → [FilePath] → IO [String]

-- The Traversable is '[ ]', and the effect is 'Maybe'
-- (String → Maybe Int) → [String] → Maybe [Int]

-- readMaybe ∷ Read a ⇒ String → Maybe a
e194 ∷ Maybe [Int]
e194 = traverse readMaybe ["1", "2", "3"]

-- |
-- >>> e194
-- Just [1,2,3]

-- e195
e195 ∷ Maybe [Int]
e195 = traverse readMaybe ["1", "snark", "3"]

-- |
-- >>> e195
-- Nothing

-- The Traversable is '((,) String)', and the effect is '[ ]'
-- (Int → [Int]) → (String, Int) → [(String, Int)]

-- e196
e196 ∷ [(String, Integer)]
e196 = traverse (\n → [n * 10, n * 100]) ("a", 10)

-- |
-- >>> e196
-- [("a",100),("a",1000)]

----------------------------
-- Traverse on Traversals --
----------------------------

-- e197
e197 ∷ Maybe (Int, Int)
e197 = traverseOf both readMaybe ("1", "2")

-- |
-- >>> e197
-- Just (1,2)

-- e198
e198 ∷ Maybe (Int, Int)
e198 = traverseOf both readMaybe ("not a number", "2")

-- |
-- >>> e198
-- Nothing

-- e199
e199 ∷ [(Char, Char)]
e199 = traverseOf both (\c → [toLower c, toUpper c]) ('a', 'b')

-- |
-- >>> e199
-- [('a','b'),('a','B'),('A','b'),('A','B')]

-- e200
e200 ∷ [(String, String)]
e200 = traverseOf (both . traversed) (\c → [toLower c, toUpper c]) ("ab", "cd")

-- |
-- >>> e200
-- [("ab","cd"),("ab","cD"),("ab","Cd"),("ab","CD"),("aB","cd"),("aB","cD"),("aB","Cd"),("aB","CD"),("Ab","cd"),("Ab","cD"),("Ab","Cd"),("Ab","CD"),("AB","cd"),("AB","cD"),("AB","Cd"),("AB","CD")]

-- simple validation
validateEmail ∷ String → Either String String
validateEmail email
  | '@' `elem` email = Right email
  | otherwise = Left ("missing '@': " <> email)

-- e201
e201 ∷ Either String [(String, String)]
e201 = traverseOf (traversed . _2) validateEmail [("Mike", "mike@tmnt.io"), ("Raph", "raph@tmnt.io"), ("Don", "don@tmnt.io"), ("Leo", "leo@tmnt.io")]

-- |
-- >>> e201
-- Right [("Mike","mike@tmnt.io"),("Raph","raph@tmnt.io"),("Don","don@tmnt.io"),("Leo","leo@tmnt.io")]

-- e202
e202 ∷ Either String [(String, String)]
e202 = traverseOf (traversed . _2) validateEmail [("Mike", "mike@tmnt.io"), ("Raph", "raph.io"), ("Don", "don@tmnt.io"), ("Leo", "leo@tmnt.io")]

-- |
-- >>> e202
-- Left "missing '@': raph.io"

-- We want to collect all the errors.
validateEmail' ∷ String → Validation [String] String
validateEmail' email
  | '@' `elem` email = Success email
  | otherwise = Failure ["missing '@': " <> email]

-- e203
e203 ∷ Validation [String] ([String], [String])
e203 = traverseOf (both . traversed) validateEmail' (["mike@tmnt.io", "raph@tmnt.io"], ["don@tmnt.io", "leo@tmnt.io"])

-- |
-- >>> e203
-- Success (["mike@tmnt.io","raph@tmnt.io"],["don@tmnt.io","leo@tmnt.io"])

-- e204
e204 ∷ Validation [String] ([String], [String])
e204 = traverseOf (both . traversed) validateEmail' (["mike@tmnt.io", "raph.io"], ["don@tmnt.io", "leo.io"])

-- |
-- >>> e204
-- Failure ["missing '@': raph.io","missing '@': leo.io"]

-- `forOf` is the arguments-flipped version of `traverseOf`.
-- forOf ∷ Traversal s t a b → s → (a → f b) → f t

-- We can use `sequenceAOf` to pull effects deep inside our structures to the outside.
-- sequenceAOf ∷ Traversal s t (f a) a → s → f t

-- Pull out `Just`.
e205 ∷ Maybe (String, String)
e205 = sequenceAOf _1 (Just "Garfield", "Lasagna")

-- |
-- >>> e205
-- Just ("Garfield","Lasagna")

-- e206
e206 ∷ Maybe (a, String)
e206 = sequenceAOf _1 (Nothing, "Lasagna")

-- |
-- >>> e206
-- Nothing

-- Pull out `Just`.
e207 ∷ Maybe ([String], [String])
e207 = sequenceAOf (both . traversed) ([Just "apples"], [Just "oranges"])

-- |
-- >>> e207
-- Just (["apples"],["oranges"])

-- e208
e208 ∷ Maybe ([String], [String])
e208 = sequenceAOf (both . traversed) ([Just "apples"], [Nothing])

-- |
-- >>> e208
-- Nothing

-----------------------------
-- Infix `traverseOf`: %%∼ --
-----------------------------

-- e209
e209 ∷ Maybe (Int, Int)
e209 = ("1", "2") & both %%~ readMaybe

-- |
-- >>> e209
-- Just (1,2)

-- e210
e210 ∷ Maybe (Int, Int)
e210 = ("not a number", "2") & both %%~ readMaybe

-- |
-- >>> e210
-- Nothing

-------------------------------
-- Using Traversals Directly --
-------------------------------

-- Instead of using `traverseOf` or `%%∼,` we can often just use the traversal itself!

-- Here we use `both` directly as though it were `traverse`:
e211 ∷ Maybe (Int, Int)
e211 = both readMaybe ("1", "2")

-- |
-- >>> e211
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

data AUser where
  AUser ∷
    { _aname ∷ String,
      _age ∷ Int
    } →
    AUser
  deriving (Show)

makeLenses ''AUser

data Account where
  Account ∷
    { _accId ∷ String,
      _user ∷ AUser
    } →
    Account
  deriving (Show)

makeLenses ''Account

--  ProdId  →   Maybe        Prod
--    |           ▼           |
--    |        traverse       |
--    |           ▼           |
-- [ProdId] →   Maybe       [Prod]

--   Int   → Either String   Int
--    |           ▼           |
--    |      traverseOf       |
--    |           ▼           |
-- Account → Either String Account

validateAge ∷ Account → Either String Account
validateAge = traverseOf (user . age) check
  where
    check ∷ Int → Either String Int
    check accAge
      | accAge < 0 = Left "Way too young!"
      | accAge > 150 = Left "Way too old!"
      | otherwise = Right accAge

-- using Maybe
-- validateAge ∷ Account → Maybe Account
-- validateAge = traverseOf (user . age) check
--   where
--     check ∷ Int → Maybe Int
--     check accAge
--       | accAge < 0 = Nothing
--       | accAge > 150 = Nothing
--       | otherwise = Just accAge

-- without traverseOf
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

-- Most optics are really just `traverse` wearing different pants.

--------------------------------
-- Our First Custom Traversal --
--------------------------------

-- Van Laarhoven optics are just a function which matches a ‘traverse-like’ signature.
-- We can write our own traversals by hand.

-- Simplified version of `traversed` which works only on lists. (`s` and `t` are lists.)
values ∷ Applicative f ⇒ (a → f b) → [a] → f [b]
values _ [] = pure []
-- values handler (a : as) = liftA2 (:) (handler a) (values handler as)
values handler (a : as) = (:) <$> handler a <*> values handler as

-- |
-- view
-- >>> ["one", "two", "three"] ^.. values
-- ["one","two","three"]

-- |
-- modify
-- >>> ["one", "two", "three"] & values %~ reverse
-- ["eno","owt","eerht"]

-- |
-- modify (changing type)
-- >>> ["one", "two", "three"] & values %~ length
-- [3,3,5]

----------------------------------
-- Traversals with Custom Logic --
----------------------------------

data Transaction where
  Withdrawal ∷ {_moneyAmount ∷ Int} → Transaction
  Deposit ∷ {_moneyAmount ∷ Int} → Transaction
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

-- Get all transactions.
e212 ∷ [Transaction]
e212 =
  let aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]
   in aliceAccount ^.. transactions . traversed

-- |
-- >>> e212
-- [Deposit {_moneyAmount = 100},Withdrawal {_moneyAmount = 20},Withdrawal {_moneyAmount = 10}]

-- Get the amounts for all transactions.
-- `moneyAmount` targets both Withdrawals and Deposits.
e213 ∷ [Int]
e213 =
  let aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]
   in aliceAccount ^.. transactions . traversed . moneyAmount

-- |
-- >>> e213
-- [100,20,10]

---------------------------------------
-- Case Study: Transaction Traversal --
---------------------------------------

-- deposits ∷ Traversal' [Transaction] Int ------------------------------------- This is what we want.
-- deposits ∷ Traversal [Transaction] [Transaction] Int Int -------------------- Expand to the simple Traversal.
-- deposits ∷ Applicative f ⇒ (Int → f Int) → [Transaction] → f [Transaction] -- Expand into the 'traverse'-like function.

-- The handler is for determining the focus.
-- Calling the handler means focus on this value.
-- Not calling the handler means do not focus on this value.
deposits ∷ Applicative f ⇒ (Int → f Int) → [Transaction] → f [Transaction]
deposits _ [] = pure []
deposits handler (Withdrawal amt : rest) = (Withdrawal amt :) <$> deposits handler rest
deposits handler (Deposit amt : rest) = liftA2 (:) (Deposit <$> handler amt) (deposits handler rest)

-- Get all the Deposit transaction amounts:
e214 ∷ [Int]
e214 = [Deposit 10, Withdrawal 20, Deposit 30] ^.. deposits

-- |
-- >>> e214
-- [10,30]

-- Multiply the amounts of all Deposits by 10
e215 ∷ [Transaction]
e215 = [Deposit 10, Withdrawal 20, Deposit 30] & deposits *~ 10

-- |
-- >>> e215
-- [Deposit {_moneyAmount = 100},Withdrawal {_moneyAmount = 20},Deposit {_moneyAmount = 300}]
isDeposit ∷ Transaction → Bool
isDeposit (Deposit _) = True
isDeposit _ = False

-- WARNING: When focusing a subset of a list like this our first thought is often to look at using a helper like `filter` to implement the Traversal.
-- But you need to be careful! `filter` is a destructive operation, it throws away any parts of the list which don't match.

-- this is what the language server found out
-- badDeposits ∷ Applicative f ⇒ (Int → f Int) → [Transaction] → f [Transaction]
badDeposits ∷ Traversal' [Transaction] Int
badDeposits handler ts = traverse go (filter isDeposit ts)
  where
    -- go ∷ Transaction → f Transaction
    go (Deposit amt) = Deposit <$> handler amt
    go (Withdrawal _) = error "This shouldn't happen"

-- e216
e216 ∷ [Int]
e216 = [Deposit 10, Withdrawal 20, Deposit 30] ^.. badDeposits

-- |
-- >>> e216
-- [10,30]

-- The `Withdrawal` is lost when using `set`.
e217 ∷ [Transaction]
e217 = [Deposit 10, Withdrawal 20, Deposit 30] & badDeposits *~ 10

-- |
-- >>> e217
-- [Deposit {_moneyAmount = 100},Deposit {_moneyAmount = 300}]

-- Using existing traversals. (This is how you would really do it!)
deposits' ∷ Traversal' [Transaction] Int
deposits' = traversed . filtered isDeposit . moneyAmount

-- e218
e218 ∷ [Transaction]
e218 = [Deposit 10, Withdrawal 20, Deposit 30] ^.. traversed . filtered isDeposit

-- |
-- >>> e218
-- [Deposit {_moneyAmount = 10},Deposit {_moneyAmount = 30}]

-- e219
e219 ∷ [Int]
e219 = [Deposit 10, Withdrawal 20, Deposit 30] ^.. deposits'

-- |
-- >>> e219
-- [10,30]

-- e220
e220 ∷ [Transaction]
e220 = [Deposit 10, Withdrawal 20, Deposit 30] & deposits' *~ 10

-- |
-- >>> e220
-- [Deposit {_moneyAmount = 100},Withdrawal {_moneyAmount = 20},Deposit {_moneyAmount = 300}]

-----------------------------------
-- Exercises - Custom Traversals --
-----------------------------------

-- 1. Rewrite the amount transaction lens manually as the following Traversal:

-- amountT ∷ Traversal' Transaction Int
amountT ∷ Applicative f ⇒ (Int → f Int) → Transaction → f Transaction
amountT handler (Withdrawal amt) = Withdrawal <$> handler amt
amountT handler (Deposit amt) = Deposit <$> handler amt

-- from dump of makeLenses
amountT' ∷ Lens' Transaction Int
amountT' f (Withdrawal amt) = Withdrawal <$> f amt
amountT' f (Deposit amt) = Deposit <$> f amt

-- 2. Reimplement the `both traversal` over tuples:

-- both ∷ Traversal (a, a) (b, b) a b
both' ∷ Applicative f ⇒ (a → f b) → (a, a) → f (b, b)
both' handler (a1, a2) = (,) <$> handler a1 <*> handler a2

-- 3. Write the following custom Traversal:

-- transactionDelta ∷ Traversal' Transaction Int
transactionDelta ∷ Functor f ⇒ (Int → f Int) → Transaction → f Transaction
transactionDelta handler (Withdrawal amt) = Withdrawal . negate <$> handler (negate amt)
transactionDelta handler (Deposit amt) = Deposit <$> handler amt

-- Applicative is a Functor
-- >>> :info Applicative
-- type Applicative ∷ (* → *) → Constraint
-- class Functor f ⇒ Applicative f where

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

-- |
-- we are in an IO context
-- >>> traverseOf badTupleSnd pure (10, "Yo")
-- (11,"Yo")

-- |
-- >>> pure (10, "Yo")
-- (10,"Yo")

-- |
-- just using Maybe as Applicative
-- >>> traverseOf badTupleSnd pure (10, "Yo") == (pure (10, "Yo") ∷ Maybe (Int, String))
-- False

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

-- using Maybe as Applicative
e221 ∷ Maybe String
e221 = traverseOf worded pure "Hello again my friend"

-- |
-- >>> e221
-- Just "Hello again my friend"

-- |
-- >>> pure "Hello again my friend" ∷ Maybe String
-- Just "Hello again my friend"

-- e222
e222 ∷ Bool
e222 = traverseOf worded pure "Hello again my friend" == (pure "Hello again my friend" ∷ Maybe String)

-- |
-- >>> e222
-- True

-- |
-- Adding a space to the string during an update creates new focuses for the worded traversal!
-- >>> ("one two" & worded %~ (<> " missisipi") & worded %~ reverse) == ("one two" & worded %~ reverse . (<> " missisipi"))
-- False

-- |
--                        space causing trouble
--                                |
-- >>> "one two" & worded %~ (<> " missisipi") & worded %~ reverse
-- "eno ipisissim owt ipisissim"

-- |
--                                  space causing trouble
--                                          |
-- >>> "one two" & worded %~ reverse . (<> " missisipi")
-- "ipisissim eno ipisissim owt"

-- |
-- removing the space
-- >>> "one two" & worded %~ (<> "missisipi") & worded %~ reverse
-- "ipisissimeno ipisissimowt"

-- |
-- >>> "one two" & worded %~ reverse . (<> "missisipi")
-- "ipisissimeno ipisissimowt"

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

-------------------------------
-- 7.7 Advanced Manipulation --
-------------------------------

-- `partsOf` is a higher-order combinator which converts a traversal into a lens over a list of the traversal's focuses:
-- partsOf ∷ Traversal' s a → Lens' s [a]

-- The lens generated by partsOf takes all the focuses of the provided traversal and packs them into a list to be manipulated.
-- Then it takes the modified list and maps each element back into the original structure!

-- Rules regarding replacement:
-- If the list has more elements than the traversal, the extras will be ignored.
-- If the list has fewer elements than the traversal, unmatched portions of the traversal will be unaffected.

-- It's because of these rules that `partsOf` isn't a polymorphic lens.
-- We might need to re-use some of the original elements so we can't change their type!

-- e223
e223 ∷ String
e223 = [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _1)

-- |
-- >>> e223
-- "abc"

-- e224
e224 ∷ [Integer]
e224 = [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _2)

-- |
-- >>> e224
-- [1,2,3]

-- We can `set` the lens to a list to replace the corresponding elements
e225 ∷ [(Char, Integer)]
e225 = [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) .~ ['c', 'a', 't']

-- |
-- >>> e225
-- [('c',1),('a',2),('t',3)]

-- Any 'extra' list elements are simply ignored
e226 ∷ [(Char, Integer)]
e226 = [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) .~ ['l', 'e', 'o', 'p', 'a', 'r', 'd']

-- |
-- >>> e226
-- [('l',1),('e',2),('o',3)]

-- Providing too few elements will keep the originals
e227 ∷ [(Char, Integer)]
e227 = [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) .~ ['x']

-- |
-- >>> e227
-- [('x',1),('b',2),('c',3)]

-- e228
e228 ∷ [(Char, Integer)]
e228 = [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) %~ reverse

-- |
-- >>> e228
-- [('c',1),('b',2),('a',3)]

-- e229
e229 ∷ [(Char, Integer)]
e229 = [('o', 1), ('o', 2), ('f', 3)] & partsOf (traversed . _1) %~ sort

-- |
-- >>> e229
-- [('f',1),('o',2),('o',3)]

-- e230
e230 ∷ [(Char, Integer)]
e230 = [('o', 1), ('o', 2), ('f', 3)] & partsOf (traversed . _1) %~ tail

-- |
-- >>> e230
-- [('o',1),('f',2),('f',3)]

-- e231
e231 ∷ (String, String, String)
e231 = ("how is a raven ", "like a ", "writing desk") & partsOf (each . traversed) %~ unwords . sort . words

-- |
-- >>> e231
-- ("a a desk how is"," like r","aven writing")

-- >>> ("how is a raven ", "like a ", "writing desk") ^.. each
-- ["how is a raven ","like a ","writing desk"]

-- >>> ("how is a raven ", "like a ", "writing desk") ^.. each . traversed
-- "how is a raven like a writing desk"

-- >>> words "how is a raven like a writing desk"
-- ["how","is","a","raven","like","a","writing","desk"]

-- >>> sort ["how","is","a","raven","like","a","writing","desk"]
-- ["a","a","desk","how","is","like","raven","writing"]

-- >>> unwords ["a","a","desk","how","is","like","raven","writing"]
-- "a a desk how is like raven writing"

-- Collect 'each' tuple element into a list, then traverse that list
e232 ∷ [String]
e232 = ("abc", "def") ^.. partsOf each . traversed

-- |
-- >>> e232
-- ["abc","def"]

-- |
-- Collect each tuple element, then traverse those strings collecting each character into a list.
-- >>> ("abc", "def") ^.. partsOf (each . traversed)
-- ["abcdef"]

-- replaces each number with its percentage of the sum of ALL numbers in the structure
e233 ∷ [(Char, Double)]
e233 = [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _2) %~ \xs → (/ sum xs) <$> xs

-- >>> [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _2)
-- [1,2,3]

-- |
-- >>> e233
-- [('a',0.16666666666666666),('b',0.3333333333333333),('c',0.5)]

-------------------------
-- Polymorphic partsOf --
-------------------------

-- We can change the type of the focus but we if set or modify with the wrong number of list elements it'll crash.
-- unsafePartsOf ∷ Traversal s t a b → Lens s t [a] [b]

-- too few elements
e234 ∷ [(Bool, Integer)]
e234 = [('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) .~ [True, False]

-- >>> e234
-- unsafePartsOf': not enough elements were supplied

-- e235
e235 ∷ [(Bool, Integer)]
e235 = [('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) .~ [True, False, True]

-- |
-- >>> e235
-- [(True,1),(False,2),(True,3)]

-- e236
e236 ∷ [((Char, Maybe Char), Integer)]
e236 = [('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) %~ \xs → zipWith (,) xs ((Just <$> tail xs) ++ [Nothing])

-- |
-- >>> e236
-- [(('a',Just 'b'),1),(('b',Just 'c'),2),(('c',Nothing),3)]

-----------------------------------------
-- `partsOf` and Other Data Structures --
-----------------------------------------

-- Interesting case study from the author.

userIds ∷ Tree UserId
userIds = undefined

lookupUsers ∷ [UserId] → IO [User]
lookupUsers = undefined

treeLookup ∷ Tree UserId → IO (Tree User)
treeLookup = traverseOf (unsafePartsOf traversed) lookupUsers

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
--                            8. Indexable Structures                                     --
--------------------------------------------------------------------------------------------

----------------------------------------
-- 8.1 What's an Indexable Structure? --
----------------------------------------

-- Each data structure provides its own separate methods for getting or setting values. Annoying!

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

---------------------------------------------------
-- 8.2 Accessing and Updating Values with 'Ixed' --
---------------------------------------------------

--------------------
-- The Ixed Class --
--------------------

-- class Ixed m where
--   ix ∷ Index m → Traversal' m (IxValue m)

-- `ix` takes an index for a structure and builds a traversal over the value at that index.

-- Index and IxValue are type families.
-- They tell us which index type or value type to use for a given structure.

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

-- Get the value at index 1:
e237 ∷ Maybe String
e237 = ["Borg", "Cardassian", "Talaxian"] ^? ix 1

-- |
-- >>> e237
-- Just "Cardassian"

-- There's no value at index 10 so the traversal doesn't focus anything.
e238 ∷ Maybe String
e238 = ["Borg", "Cardassian", "Talaxian"] ^? ix 10

-- |
-- >>> e238
-- Nothing

-- It's a traversal, so we can `set` new values at that index.
e239 ∷ [String]
e239 = ["Borg", "Cardassian", "Talaxian"] & ix 1 .~ "Vulcan"

-- |
-- >>> e239
-- ["Borg","Vulcan","Talaxian"]

-- A `set` will do nothing if the given index doesn't have a value.
e240 ∷ [String]
e240 = ["Borg", "Cardassian", "Talaxian"] & ix 10 .~ "Romulan"

-- |
-- >>> e240
-- ["Borg","Cardassian","Talaxian"]

-- Get the value at key "Zuko".
e241 ∷ Maybe String
e241 = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] ^? ix "Zuko"

-- |
-- >>> e241
-- Just "Fire"

-- If there's no value at a key, the traversal returns zero elements.
e242 ∷ Maybe String
e242 = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] ^? ix "Sokka"

-- |
-- >>> e242
-- Nothing

-- We can set the value at a key, but only if that key already exists.
e243 ∷ Map String String
e243 = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & ix "Toph" .~ "Metal"

-- |
-- >>> e243
-- fromList [("Katara","Water"),("Toph","Metal"),("Zuko","Fire")]

-- Setting a non-existent element of a Map does NOT insert it.
e244 ∷ Map String String
e244 = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & ix "Iroh" .~ "Lightning"

-- |
-- >>> e244
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

e245 ∷ Maybe Char
e245 = T.pack "hello" ^? ix 0

-- |
-- >>> e245
-- Just 'h'
e246 ∷ Maybe Char
e246 = T.pack "hello" ^? ix 10

-- |
-- >>> e246
-- Nothing
e247 ∷ Text
e247 = T.pack "hello" & ix 0 .~ 'j'

-- |
-- >>> e247
-- "jello"
packStr ∷ String → B.ByteString
packStr = encodeUtf8 . T.pack

-- Word8's are shown as integers
e248 ∷ Maybe Word8
e248 = packStr "hello" ^? ix 0

-- |
-- >>> e248
-- Just 104

-- e249
e249 ∷ Maybe Word8
e249 = packStr "hello" ^? ix 10

-- |
-- >>> e249
-- Nothing

-- We can edit a Word8 within a ByteString as though it's an integer.
e250 ∷ ByteString
e250 = packStr "hello" & ix 0 +~ 2

-- |
-- >>> e250
-- "jello"

-- We can always 'traverse' a traversal using effectful handlers!
e251 ∷ [Text]
e251 = T.pack "hello" & ix 1 %%~ const "aeiou"

-- |
-- >>> e251
-- ["hallo","hello","hillo","hollo","hullo"]

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

-- The empty list represents the value at the root.
-- 0 → left, 1 → right, empty → stay
e252 ∷ Maybe Int
e252 = tree ^? ix []

-- |
-- >>> e252
-- Just 1

-- 0 represents the first child of the root.
e253 ∷ Maybe Int
e253 = tree ^? ix [0]

-- |
-- >>> e253
-- Just 2

-- 0, 0 descends into the first child twice then focuses the value at that node.
e254 ∷ Maybe Int
e254 = tree ^? ix [0, 0]

-- |
-- >>> e254
-- Just 4

-- e255
e255 ∷ Maybe Int
e255 = tree ^? ix [1, 0]

-- |
-- >>> e255
-- Just 5

-- e256
e256 ∷ Maybe Int
e256 = tree ^? ix [1, 1]

-- |
-- >>> e256
-- Just 6

-- Invalid paths simply return an empty traversal.
e257 ∷ Maybe Int
e257 = tree ^? ix [5, 6]

-- |
-- >>> e257
-- Nothing

-- Indexing into a function runs the function with the index as input
e258 ∷ Maybe String
e258 = reverse ^? ix "Stella!"

-- |
-- >>> e258
-- Just "!alletS"

-- Invalid paths simply return an empty traversal.
-- We can set or traverse individual results of a function!
-- Here we overwrite the function's output at the input value "password" so it instead returns a new value.

specialReverse ∷ String → String
specialReverse = reverse & ix "password" .~ "You found the secret!"

e259 ∷ String
e259 = specialReverse "password"

-- |
-- >>> e259
-- "You found the secret!"

-- The function is unaffected at all other inputs
e260 ∷ String
e260 = specialReverse "dunno"

-- |
-- >>> e260
-- "onnud"

----------------------------------------
-- 8.3 Inserting & Deleting with `at` --
----------------------------------------

-------------------------
-- Map-like Structures --
-------------------------

-- The At typeclass allows focusing values within map-like structures which allow arbitrary insertion or deletion.
-- This is not possible with lists. You can't insert a 10th element unless you have a 9th!

-- class At where
--   at ∷ Index m → Lens' m (Maybe (IxValue m))

-- For comparison, here's `ix` and `at` side-by-side:
-- ix ∷ Index m → Traversal' m        (IxValue m)
-- at ∷ Index m → Lens'      m (Maybe (IxValue m))

-- To insert or replace an element we can set a value wrapped in Just; to delete we can set the focus to Nothing.

-- e261
e261 ∷ Map String String
e261 = M.insert "Mikey" "Nunchaku" $ M.fromList [("Leo", "Katanas"), ("Raph", "Sai")]

-- |
-- >>> e261
-- fromList [("Leo","Katanas"),("Mikey","Nunchaku"),("Raph","Sai")]
e262 ∷ Map String String
e262 = M.delete "Leo" $ M.fromList [("Leo", "Katanas"), ("Raph", "Sai")]

-- |
-- >>> e262
-- fromList [("Raph","Sai")]

-- We can update a value arbitrarily using `over` and providing a function from `Maybe a → Maybe a`.

-- Since 'at' creates a lens, we use `^.` instead of `^?`
e263 ∷ Maybe String
e263 = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] ^. at "Zuko"

-- |
-- >>> e263
-- Just "Fire"

-- to delete an element we can set the focus to Nothing
e264 ∷ Map String String
e264 = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & at "Zuko" .~ Nothing

-- |
-- >>> e264
-- fromList [("Katara","Water"),("Toph","Earth")]

-- to insert/replace an element we can set a value wrapped in Just
e265 ∷ Map String String
e265 = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & at "Iroh" .~ Just "Lightning"

-- |
-- >>> e265
-- fromList [("Iroh","Lightning"),("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]

-- ?~ is for inserting/updating elements
-- Using the `?~` operator we can avoid writing out the Just when inserting elements.
e266 ∷ Map String String
e266 = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")] & at "Iroh" ?~ "Lightning"

-- |
-- >>> e266
-- fromList [("Iroh","Lightning"),("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]

-- We can use `?~` with other optics if we want, but it's usually used with `at`.
e267 ∷ (Maybe String, Maybe String)
e267 = (1, 2) & both .~ Just "twins!"

-- |
-- >>> e267
-- (Just "twins!",Just "twins!")

-- We can use `?~` with other optics if we want, but it's usually used with `at`.
e268 ∷ (Maybe String, Maybe String)
e268 = (1, 2) & both ?~ "twins!"

-- |
-- >>> e268
-- (Just "twins!",Just "twins!")

-- `sans` deletes elements
-- `sans` is just short-hand for setting the value at an index to `Nothing`.
e269 ∷ Map String String
e269 = sans "Katara" $ M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]

-- |
-- >>> e269
-- fromList [("Toph","Earth"),("Zuko","Fire")]

-----------------------
-- Manipulating Sets --
-----------------------

-- One way to imagine a Set is as a map where the set elements are keys: (Map v ()).
-- Using unit `()` as the value means the only real information stored in the Map is whether a value exists or not.
-- Just () means it exists, Nothing means it does not exist.

primes ∷ Set Int
primes = S.fromList [2, 3, 5, 7, 11, 13]

-- e270
e270 ∷ Maybe ()
e270 = primes ^? ix 5

-- |
-- >>> e270
-- Just ()

-- e271
e271 ∷ Maybe ()
e271 = primes ^? ix 4

-- |
-- >>> e271
-- Nothing

-- Insert 17 into the Set.
e272 ∷ Set Int
e272 = primes & at 17 ?~ ()

-- |
-- >>> e272
-- fromList [2,3,5,7,11,13,17]

-- Remove 4 from the Set.
e273 ∷ Set Int
e273 = sans 5 primes

-- |
-- >>> e273
-- fromList [2,3,7,11,13]

-- We can use `&` to chain uses of `sans`.
e274 ∷ Set Int
e274 = primes & sans 5 & sans 7 & sans 11

-- |
-- >>> e274
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

----------------------------------------
-- 8.4 Custom Indexed Data Structures --
----------------------------------------

------------------------------------
-- Custom Ixed: Cyclical Indexing --
------------------------------------

newtype Cycled a = Cycled [a]
  deriving (Show)

type instance Index (Cycled a) = Int

type instance IxValue (Cycled a) = a

-- there is a default implementation in terms of `at`
-- >>> :info ix
-- type Ixed ∷ * → Constraint
-- class Ixed m where
--   ix ∷ Index m → Traversal' m (IxValue m)
--   default ix ∷ At m ⇒ Index m → Traversal' m (IxValue m)

-- >>> :info Traversal'
-- type Traversal' ∷ * → * → *
-- type Traversal' s a = Traversal s s a a

-- >>> :info Traversal
-- type Traversal ∷ * → * → * → * → *
-- type Traversal s t a b =
--   ∀ (f ∷ * → *). Applicative f ⇒ (a → f b) → s → f t

instance Ixed (Cycled a) where
  -- ix ∷ Index m → Traversal' m (IxValue m)
  -- ix ∷ Index (Cycled a) → Traversal' (Cycled a) (IxValue (Cycled a))
  -- ix ∷ Int              → Traversal' (Cycled a)  a
  ix ∷ Applicative f ⇒ Int → (a → f a) → Cycled a → f (Cycled a)
  ix i handler (Cycled xs) = Cycled <$> traverseOf (ix (i `mod` length xs)) handler xs

-- e275
e275 ∷ Maybe Char
e275 = Cycled ['a', 'b', 'c'] ^? ix 1

-- |
-- >>> e275
-- Just 'b'

-- e276
e276 ∷ Maybe Char
e276 = Cycled ['a', 'b', 'c'] ^? ix 3

-- |
-- >>> e276
-- Just 'a'

-- e277
e277 ∷ Maybe Char
e277 = Cycled ['a', 'b', 'c'] ^? ix 10

-- |
-- >>> e277
-- Just 'b'

-- e278
e278 ∷ Maybe Char
e278 = Cycled ['a', 'b', 'c'] ^? ix (-1)

-- |
-- >>> e278
-- Just 'c'

-- e279
e279 ∷ Cycled Char
e279 = Cycled ['a', 'b', 'c'] & ix 0 .~ '!'

-- |
-- >>> e279
-- Cycled "!bc"

-- e280
e280 ∷ Cycled Char
e280 = Cycled ['a', 'b', 'c'] & ix 10 .~ '!'

-- |
-- >>> e280
-- Cycled "a!c"

-- e281
e281 ∷ Cycled Char
e281 = Cycled ['a', 'b', 'c'] & ix (-1) .~ '!'

-- |
-- >>> e281
-- Cycled "ab!"

---------------------------------
-- Custom At: Address Indexing --
---------------------------------

-- The data type doesn't need to be a traditional "container type".
-- So long as we can implement the typeclass it'll all work!

data PostalAddress where
  PostalAddress ∷
    { _buildingNumber ∷ Maybe String,
      _streetsName ∷ Maybe String,
      _apartmentNumber ∷ Maybe String,
      _postalCode ∷ Maybe String
    } →
    PostalAddress
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
-- It will be implemented automatically in terms of `at`.

-- >>> :info ix
-- type Ixed ∷ * → Constraint
-- class Ixed m where
--   ix ∷ Index m → Traversal' m (IxValue m)
--   default ix ∷ At m ⇒ Index m → Traversal' m (IxValue m)

instance Ixed PostalAddress

-- >>> :info at
-- type At ∷ * → Constraint
-- class Ixed m ⇒ At m where
--   at ∷ Index m → Lens' m (Maybe (IxValue m))
--   	-- Defined in ‘Control.Lens.At’

instance At PostalAddress where
  -- at ∷ Index m → Lens' m (Maybe (IxValue m))
  -- at ∷ Index PostalAddress → Lens' PostalAddress (Maybe (IxValue PostalAddress))
  -- at ∷ AddressPiece        → Lens' PostalAddress (Maybe  String)
  at ∷ AddressPiece → Lens' PostalAddress (Maybe String)
  at BuildingNumber = buildingNumber
  at StreetName = streetsName
  at ApartmentNumber = apartmentNumber
  at PostalCode = postalCode

-- e282
e282 ∷ PostalAddress
e282 = PostalAddress Nothing Nothing Nothing Nothing

-- |
-- >>> e282
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Nothing, _apartmentNumber = Nothing, _postalCode = Nothing}

-- e283
e283 ∷ PostalAddress
e283 = PostalAddress Nothing Nothing Nothing Nothing & at StreetName ?~ "Baker St." & at ApartmentNumber ?~ "221B"

-- |
-- >>> e283
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Just "Baker St.", _apartmentNumber = Just "221B", _postalCode = Nothing}

-- e284
e284 ∷ PostalAddress
e284 = PostalAddress Nothing Nothing Nothing Nothing & at StreetName ?~ "Baker St." & at ApartmentNumber ?~ "221B" & ix ApartmentNumber .~ "221A"

-- |
-- >>> e284
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Just "Baker St.", _apartmentNumber = Just "221A", _postalCode = Nothing}

-- e285
e285 ∷ PostalAddress
e285 = PostalAddress Nothing Nothing Nothing Nothing & at StreetName ?~ "Baker St." & at ApartmentNumber ?~ "221B" & sans StreetName

-- |
-- >>> e285
-- PostalAddress {_buildingNumber = Nothing, _streetsName = Nothing, _apartmentNumber = Just "221B", _postalCode = Nothing}

-------------------------------------------
-- Exercises – Custom Indexed Structures --
-------------------------------------------

-- 1. Implement both Ixed and At for a newtype wrapper around a Map which makes indexing case insensitive, you can specialize to String or Text keys.
--    Write the ix instance manually even though it has a default implementation.
--    It's okay to assume that user will only interact with the map via Ixed and At.

newtype CaseInsensitive value = CaseInsensitive {_unCaseSensitive ∷ M.Map String value} deriving (Show)

makeLenses ''CaseInsensitive

type instance Index (CaseInsensitive value) = String

type instance IxValue (CaseInsensitive value) = value

strToLower ∷ String → String
strToLower = map toLower

-- >>> :info Traversal
-- type Traversal s t a b = ∀ (f ∷ * → *). Applicative f ⇒ (a → f b) → s → f t

-- >>> :info ix
-- class Ixed m where
--   ix ∷ Index m → Traversal' m (IxValue m)

-- TODO
instance Ixed (CaseInsensitive value) where
  -- ix ∷ Index (CaseInsensitive value) → Traversal' (CaseInsensitive value) (IxValue (CaseInsensitive value))
  -- ix ∷ String → Traversal' (CaseInsensitive value) value
  ix ∷ Applicative f ⇒ String → (value → f value) → CaseInsensitive value → f (CaseInsensitive value)
  -- ix key handler ci = ix (strToLower key) handler ci
  -- ix key = ix (strToLower key)
  ix key = unCaseSensitive . ix (strToLower key)

-- >>> :info at
-- type At ∷ * → Constraint
-- class Ixed m ⇒ At m where
--   at ∷ Index m → Lens' m (Maybe (IxValue m))

-- >>> :info Lens
-- type Lens s t a b = ∀ (f ∷ * → *). Functor f ⇒ (a → f b) → s → f t

instance At (CaseInsensitive value) where
  -- at ∷ Index (CaseInsensitive value) → Lens' (CaseInsensitive value) (Maybe (IxValue (CaseInsensitive value)))
  at ∷ String → Lens' (CaseInsensitive value) (Maybe value)
  -- at key = at (strToLower key)
  at key = unCaseSensitive . at (strToLower key)

---------------------------------
-- 8.5 Handling Missing Values --
---------------------------------

--------------------------------------
-- Checking whether Updates Succeed --
--------------------------------------

-- `failover` action

-- actual type signature
-- failover ∷ Alternative m ⇒ LensLike ((,) Any) s t a b → (a → b) → s → m t

-- specialized type signature
-- failover ∷ Traversal s t a b → (a → b) → s → Maybe t

-- It takes an update function and will apply it to all focuses.
-- If it doesn't find any focuses at all then it will return the
-- value of empty for whichever Alternative type is inferred as
-- the return value, but we usually use Nothing from Maybe.

-- there's no element at index 6, so the update fails
e286 ∷ Maybe String
e286 = "abcd" & failover (ix 6) toUpper

-- |
-- >>> e286
-- Nothing

-- There's an element at index 2, so the update succeeds!
e287 ∷ Maybe String
e287 = "abcd" & failover (ix 2) toUpper

-- |
-- >>> e287
-- Just "abCd"

-- e288
e288 ∷ Maybe [Int]
e288 = [] & failover _head (* 10)

-- |
-- >>> e288
-- Nothing

-- We can nest traversals; the whole chain fails if it focuses no elements.
e289 ∷ Maybe (M.Map Char [Int])
e289 = M.fromList [('a', [1, 2])] & failover (ix 'a' . ix 1) (* 10)

-- |
-- >>> e289
-- Just (fromList [('a',[1,20])])

-- e290
e290 ∷ Maybe (M.Map Char [Int])
e290 = M.fromList [('a', [1, 2])] & failover (ix 'a' . ix 3) (* 10)

-- |
-- >>> e290
-- Nothing

-- It even works with filters.
e291 ∷ Maybe [Int]
e291 = [1, 3, 5] & failover (traversed . filtered odd) (* 10)

-- |
-- >>> e291
-- Just [10,30,50]

-- e292
e292 ∷ Maybe [Int]
e292 = [1, 3, 5] & failover (traversed . filtered even) (* 10)

-- |
-- >>> e292
-- Nothing

-- Note the Alternative constraint on `failover`.
-- >>> :info failover
-- failover ∷ Alternative m ⇒ LensLike ((,) Any) s t a b → (a → b) → s → m t

e293 ∷ Maybe String
e293 =
  let s = "abcdefg"
   in failover (ix 8) toUpper s <|> failover (ix 6) toUpper s <|> failover (ix 4) toUpper s

-- |
-- >>> e293
-- Just "abcdefG"

------------------------------
-- Fallbacks with `failing` --
------------------------------

-- Making choices based on success or failure within an optics path itself!

-- `failing` combines two optics by trying the first, and falling back on the second.

-- specialized signatures
-- failing ∷ Fold      s t a b → Fold      s t a b → Fold      s t a b
-- failing ∷ Traversal s t a b → Traversal s t a b → Traversal s t a b

-- Try to get something from index 10, failing that, get something from index 2.
e294 ∷ Maybe Integer
e294 = M.fromList [('a', 1), ('b', 2)] ^? (ix 'z' `failing` ix 'b')

-- |
-- >>> e294
-- Just 2

-- It works with updates as well:
e295 ∷ Map Char Integer
e295 = M.fromList [('a', 1), ('b', 2)] & (ix 'z' `failing` ix 'b') *~ 10

-- |
-- >>> e295
-- fromList [('a',1),('b',20)]

-- Get the first album available in the map in order of preference.
e296 ∷ Maybe String
e296 = M.fromList [("Bieber", "Believe"), ("Beyoncé", "Lemonade")] ^? (ix "Swift" `failing` ix "Bieber" `failing` ix "Beyoncé")

-- |
-- >>> e296
-- Just "Believe"

-- The optics we pass to `failing` can be arbitrarily complex so long as the type of the focus is the same for each argument.
e297 ∷ [Integer]
e297 = M.fromList [('a', (1, [2, 3, 4])), ('b', (5, [6, 7, 8]))] ^.. (ix 'z' . _1 `failing` ix 'a' . _2 . ix 10 `failing` ix 'b' . _2 . traversed)

-- |
-- >>> e297
-- [6,7,8]
e298 ∷ [Integer]
e298 = M.fromList [('a', (1, [2, 3, 4])), ('b', (5, [6, 7, 8]))] ^.. (ix 'z' . _1 `failing` ix 'a' . _2 . ix 1 `failing` ix 'b' . _2 . traversed)

-- |
-- >>> e298
-- [3]

----------------------------------
-- Default Elements using `non` --
----------------------------------

-- We configure `non` by passing it a default value.
-- It uses the default value to build a traversal which focuses the value within a Just,
-- or in the case of a Nothing focuses the default value instead.

-- simplified
-- non ∷ Eq a ⇒ a → Traversal' (Maybe a) a

-- This mimics the behavior of `fromMaybe` using optics.
-- >>> :type fromMaybe
-- fromMaybe ∷ a → Maybe a → a

-- e299
e299 ∷ Integer
e299 = Nothing ^. non 0

-- |
-- >>> e299
-- 0

-- e300
e300 ∷ String
e300 = Nothing ^. non "default"

-- |
-- >>> e300
-- "default"

-- e301
e301 ∷ String
e301 = Just "value" ^. non "default"

-- |
-- >>> e301
-- "value"

-- `non` becomes much more useful when combined with the `at` combinator.

-- 'at' focuses a value wrapped in Maybe, and Nothing is used if the value is missing.
-- This behaviour dovetails with non to substitute a default value for missing values.

-- >>> :info at
-- class Ixed m ⇒ At m where
--   at ∷ Index m → Lens' m (Maybe (IxValue m))

-- Using `non` unwraps the `Maybe` so we can view values directly.
-- "Leslie" has a favourite food, so we can look it up:
e302 ∷ String
e302 =
  let favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
   in favouriteFoods ^. at "Leslie" . non "Pizza"

-- |
-- >>> e302
-- "Waffles"

-- If we don't know someone's favourite food, the default is "Pizza"
e303 ∷ String
e303 =
  let favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
   in favouriteFoods ^. at "Leo" . non "Pizza"

-- |
-- >>> e303
-- "Pizza"

-- When setting through `non` it allows us to set values directly without worrying about wrapping them in `Just`.
e304 ∷ Map String String
e304 =
  let favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
   in favouriteFoods & at "Popeye" . non "Pizza" .~ "Spinach"

-- |
-- >>> e304
-- fromList [("Garfield","Lasagna"),("Leslie","Waffles"),("Popeye","Spinach")]

-- e305
e305 ∷ Map String String
e305 =
  let favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
   in favouriteFoods & at "Leslie" . non "Pizza" .~ "Spinach"

-- |
-- >>> e305
-- fromList [("Garfield","Lasagna"),("Leslie","Spinach")]

-- `non` has interesting behavior when setting a key to the default value.
-- It maps the default value back to Nothing, which `at` will be exclude from the map.
-- If something is the default value it's redundant to store it.
-- If we try to view that key later we'll still receive the correct value by using the default.
-- We can use this to build sparse maps and save some performance if one particular value in the map is very common.

-- "Garfield" isn't stored when his favourite matches the default.
e306 ∷ Map String String
e306 =
  let fav name = at name . non "Pizza"
      favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
   in favouriteFoods & fav "Garfield" .~ "Pizza"

-- |
-- >>> e306
-- fromList [("Leslie","Waffles")]

-- We still get the correct value when retrieving Garfield's favourite.
e307 ∷ String
e307 =
  let fav name = at name . non "Pizza"
      favouriteFoods = M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
      newFavourites = favouriteFoods & fav "Garfield" .~ "Pizza"
   in newFavourites ^. fav "Garfield"

-- |
-- >>> e307
-- "Pizza"

-- Let's keep tally of the number of hours each employee has worked in a Map.
-- If we add hours for an employee missing from the map they should be added as though they had logged 0 hours.

-- Erin will be added to the map since she's missing.
e308 ∷ Map String Integer
e308 = M.fromList [("Jim", 32), ("Dwight", 39)] & at "Erin" . non 0 +~ 10

-- |
-- >>> e308
-- fromList [("Dwight",39),("Erin",10),("Jim",32)]

-- Since Jim already has hours logged we simply increment them.
e309 ∷ Map String Integer
e309 = M.fromList [("Jim", 32), ("Dwight", 39)] & at "Jim" . non 0 +~ 8

-- |
-- >>> e309
-- fromList [("Dwight",39),("Jim",40)]

-- When we pay-out an employee's hours, we set their hours to `0`
-- `non` removes any keys with the default value from the list entirely!
e310 ∷ Map String Integer
e310 = M.fromList [("Jim", 32), ("Dwight", 39)] & at "Dwight" . non 0 .~ 0

-- |
-- >>> e310
-- fromList [("Jim",32)]

-----------------------------------
-- Checking Fold Success/Failure --
-----------------------------------

-- e311
e311 ∷ Char
e311 = fromMaybe 'z' ("abc" ^? ix 1)

-- |
-- >>> e311
-- 'b'

-- e312
e312 ∷ Char
e312 = fromMaybe 'z' ("abc" ^? ix 10)

-- |
-- >>> e312
-- 'z'

-- We can do something similar with `pre`.

-- `pre` is a version of `preview` as a higher-order optic.
-- You pass it a fold and it will try to get a value from it, returning `Nothing` if it can't find one.
-- Note that it returns a Getter, so we can't set or update using `pre`.

-- specialized signature
-- pre ∷ Fold s a → Getter s (Maybe a)

-- We can combine this with `non` and `ix` to get default values when accessing list elements.
e313 ∷ Char
e313 = "abc" ^. pre (ix 10) . non 'z'

-- |
-- >>> e313
-- 'z'

-- We use ^. rather than ^? since `pre` turns the fold into a Getter.
e314 ∷ Maybe Integer
e314 = [1, 2, 3, 4] ^. pre (traversed . filtered even)

-- |
-- >>> e314
-- Just 2

-- e315
e315 ∷ Maybe Integer
e315 = [1, 3] ^. pre (traversed . filtered even)

-- |
-- >>> e315
-- Nothing

-- `pre` isn't very commonly used.

--------------------------------
-- Exercises - Missing Values --
--------------------------------

-- 1. Write an optic which focuses the value at key "first" or, failing that, the value at key "second".

-- e316
e316 ∷ Map String Bool
e316 =
  let optic = ix "first" `failing` ix "second"
   in M.fromList [("first", False), ("second", False)] & optic .~ True

-- |
-- >>> e316
-- fromList [("first",True),("second",False)]

-- e317
e317 ∷ Map String Bool
e317 =
  let optic = ix "first" `failing` ix "second"
   in M.fromList [("no-first", False), ("second", False)] & optic .~ True

-- |
-- >>> e317
-- fromList [("no-first",False),("second",True)]

-- e318
e318 ∷ Map String Bool
e318 =
  let optic = ix "first" `failing` ix "second"
   in M.fromList [("second", False)] & optic .~ True

-- |
-- >>> e318
-- fromList [("second",True)]

-- 2. Write an optic which focuses the first element of a tuple iff it is even, and the second tuple element otherwise.
--    Assume each slot contains an integer.

-- e319
e319 ∷ (Integer, Integer)
e319 =
  let optic = ix 0 . filtered even `failing` ix 1
   in (2, 2) & optic *~ 10

-- |
-- >>> e319
-- (20,2)

-- e320
e320 ∷ (Integer, Integer)
e320 =
  let optic = ix 0 . filtered even `failing` ix 1
   in (1, 1) & optic *~ 10

-- |
-- >>> e320
-- (1,10)

-- 3. Write an optic which focuses all even numbers in a list, if none of the members are even then focus ALL numbers in the list.

-- e321
e321 ∷ [Integer]
e321 =
  let optic = traversed . filtered even `failing` traversed
   in [1, 2, 3, 4] ^.. optic

-- |
-- >>> e321
-- [2,4]

-- e322
e322 ∷ [Integer]
e322 =
  let optic = traversed . filtered even `failing` traversed
   in [1, 3, 5] ^.. optic

-- |
-- >>> e322
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

-- e323
e323 ∷ String
e323 = ["Math", "Science", "Geography"] ^. pre (ix 100) . non "Unscheduled"

-- |
-- >>> e323
-- "Unscheduled"

-- |
--     ["Math", "Science", "Geography"] ^. _            . non "Unscheduled"
-- >>> ["Math", "Science", "Geography"] ^. pre (ix 100) . non "Unscheduled"
-- "Unscheduled"

-- BONUS: Excellent example for `pre`!
-- Use 'pre' and 'non'

-- e324
e324 ∷ [Integer]
e324 = [1, 2, 3, 4] ^.. traversed . pre (filtered even) . non (-1)

-- |
-- >>> e324
-- [-1,2,-1,4]

-- >>> [1, 2, 3, 4] ^.. traversed . pre (filtered even) -- . non (-1)
-- [Nothing,Just 2,Nothing,Just 4]

-- |
--     [1, 2, 3, 4] ^.. _
-- >>> [1, 2, 3, 4] ^.. traversed . pre (filtered even) . non (-1)
-- [-1,2,-1,4]

--------------------------------------------------------------------------------------------
--                                      9. Prisms                                         --
--------------------------------------------------------------------------------------------

--------------------------------
-- 9.1 Introduction to Prisms --
--------------------------------

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
-- We'll see how prisms are a natural candidate for specifying PATTERN-MATCHING semantics and help to work with sum types as well.

-- For running a Prism forwards  run `preview` (^?).
-- For running a Prism backwards run `review`  (#).
--
-- review = reverse view

------------------------------------
-- Simple Pattern-Matching Prisms --
------------------------------------

-- For the `Either` type we have `_Left` and `_Right` prisms.
-- _Left  ∷ Prism (Either l r) (Either l' r ) l l'
-- _Right ∷ Prism (Either l r) (Either l  r') r r'

-- These prisms will pattern match on the Either type, focusing on the value inside.
-- We can use `preview` (^?) to "run" the pattern match, returning Nothing if it's not a match:

-- e325
e325 ∷ Maybe String
e325 = Left "message" ^? _Left

-- |
-- >>> e325
-- Just "message"

-- e326
e326 ∷ Maybe a
e326 = Left "message" ^? _Right

-- |
-- >>> e326
-- Nothing

-- e327
e327 ∷ Maybe Integer
e327 = Right 42 ^? _Right

-- |
-- >>> e327
-- Just 42

-- e328
e328 ∷ Maybe a
e328 = Right 42 ^? _Left

-- |
-- >>> e328
-- Nothing

-- Since prisms are valid traversals we can set, update, or traverse the focused value through them.
e329 ∷ Either Integer b
e329 = Left 10 & _Left +~ 5

-- |
-- >>> e329
-- Left 15

-- e330
e330 ∷ Either a String
e330 = Right "howdy" & _Right %~ reverse

-- |
-- >>> e330
-- Right "ydwoh"

-- Analogously for `Maybe`.
-- _Nothing ∷ Prism' (Maybe a) ()
-- _Just    ∷ Prism  (Maybe a) (Maybe b) a b

-- e331
e331 ∷ Maybe ()
e331 = Nothing ^? _Nothing

-- |
-- >>> e331
-- Just ()

-- e332
e332 ∷ Maybe a
e332 = Nothing ^? _Just

-- |
-- >>> e332
-- Nothing

-- e333
e333 ∷ Maybe String
e333 = Just "gold" ^? _Just

-- |
-- >>> e333
-- Just "gold"

-- e334
e334 ∷ Maybe ()
e334 = Just "gold" ^? _Nothing

-- |
-- >>> e334
-- Nothing

-- e335
e335 ∷ Maybe Integer
e335 = Just 20 & _Just %~ (+ 10)

-- |
-- >>> e335
-- Just 30

-----------------------------------------------------------
-- Checking Pattern Matches with Prisms (`has`, `isn't`) --
-----------------------------------------------------------

-- has   ∷ Fold  s   a   → s → Bool
-- isn't ∷ Prism s t a b → s → Bool

-- There's also an `is` to match `isn't`, but that's hidden away in the Control.Lens.Extras module to avoid naming conflicts.
-- So usually it's more convenient to use `has`.

-- `has` checks whether the given fold yields any elements when run on the provided structure
-- `isn't` returns whether a given prism does not match the input.

-- e336
e336 ∷ Bool
e336 = has _Right (Left "message")

-- |
-- >>> e336
-- False

-- e337
e337 ∷ Bool
e337 = has _Right (Right 37)

-- |
-- >>> e337
-- True

-- e338
e338 = isn't _Nothing (Just 12)

-- |
-- >>> e338
-- True

-- e339
e339 ∷ Bool
e339 = isn't _Left (Left ())

-- |
-- >>> e339
-- False

-----------------------------------------
-- Generating Prisms with `makePrisms` --
-----------------------------------------

-- simplified representation of HTTP web requests

-- A path is a list of URL segments.
type Path = [String]

type Body = String

data Request where
  Post ∷ Path → Body → Request
  Get ∷ Path → Request
  Delete ∷ Path → Request
  deriving (Show)

-- Creates `_Post` `_Get` and `_Delete` prisms.
makePrisms ''Request

-- `:browse` in the REPL:
-- _Post   ∷ Prism' Request (Path, Body)  -- Note the tuple!
-- _Get    ∷ Prism' Request  Path
-- _Delete ∷ Prism' Request  Path

-- This is all we need to use these prisms for getting or setting as if they were traversals.

-- e340
e340 ∷ Maybe Path
e340 = Get ["users"] ^? _Get

-- |
-- >>> e340
-- Just ["users"]

-- e341
e341 ∷ Maybe (Path, Body)
e341 = Get ["users"] ^? _Post

-- |
-- >>> e341
-- Nothing

-- e342
e342 ∷ Request
e342 = Get ["users"] & _Get .~ ["posts"]

-- |
-- >>> e342
-- Get ["posts"]

-- e343
e343 ∷ Maybe (Path, Body)
e343 = Post ["users"] "name: John" ^? _Post

-- |
-- >>> e343
-- Just (["users"],"name: John")

-- e344
e344 ∷ Request
e344 = Post ["users"] "name: John" & _Post . _1 <>~ ["12345"]

-- |
-- >>> e344
-- Post ["users","12345"] "name: John"

----------------------------------
-- Embedding Values with Prisms --
----------------------------------

-- Every prism represents a pattern-match which can be REVERSED!
-- By feeding a prism a focus we can embed that focus into a structure via the context implied by the pattern!
-- Even though viewing through a prism may fail if the pattern doesn't match, embedding a value into a pattern using a prism always succeeds!
-- To run a prism backwards we use the `review` (#), which you can think of as short for "reverse view". It embeds the focus into the prism's pattern.

-- (#) is comparable to the pipe operator in PureScript.
-- PureScript's (#) is (&) in Haskell.

-- review ∷ Prism s t a b → b → t
-- (#)    ∷ Prism s t a b → b → t

-- Prisms which match on a constructor of some type can be reversed to embed fields into the constructor.
-- The reverse of unpacking a specific constructor is to pack those fields into that constructor.

-- e345
e345 ∷ Request
e345 = Get ["posts"]

-- |
-- >>> e345
-- Get ["posts"]

-- e346
e346 ∷ Request
e346 = review _Get ["posts"]

-- |
-- >>> e346
-- Get ["posts"]

-- e347
e347 ∷ Request
e347 = _Get # ["posts"]

-- |
-- >>> e347
-- Get ["posts"]

-- e348
e348 ∷ Request
e348 = Delete ["posts"]

-- |
-- >>> e348
-- Delete ["posts"]

-- e349
e349 ∷ Request
e349 = _Delete # ["posts"]

-- |
-- >>> e349
-- Delete ["posts"]

-- e350
e350 ∷ Request
e350 = Post ["posts"] "My blog post"

-- |
-- >>> e350
-- Post ["posts"] "My blog post"

-- Constructors with multiple fields accept a tuple of the fields.
e351 ∷ Request
e351 = _Post # (["posts"], "My blog post")

-- |
-- >>> e351
-- Post ["posts"] "My blog post"

-- e352
e352 ∷ Either String a
e352 = Left "an error"

-- |
-- >>> e352
-- Left "an error"

-- constructing a `Left` from a string
e353 ∷ Either String a
e353 = review _Left "an error"

-- |
-- >>> e353
-- Left "an error"

-- constructing a `Left` from a string
e354 ∷ Either [Char] c
e354 = _Left # "an error"

-- |
-- >>> e354
-- Left "an error"

-- e355
e355 ∷ Either a Integer
e355 = Right 42

-- |
-- >>> e355
-- Right 42

-- e356
e356 ∷ Either a Integer
e356 = review _Right 42

-- |
-- >>> e356
-- Right 42

-- e357
e357 ∷ Either a Integer
e357 = _Right # 42

-- |
-- >>> e357
-- Right 42

-- Since composing prisms results in a new prism, we can compose prisms before passing them to review to build a nested constructor function.
e358 ∷ Maybe (Either Integer a)
e358 = _Just . _Left # 1337

-- |
-- >>> e358
-- Just (Left 1337)

-- We could and should use the normal constructors.
-- But, the ability to reverse a prism can be used to implement some of the prism combinators we'll look at later on.

-----------------------------
-- Other Types of Patterns --
-----------------------------

-- Although most of the prisms you'll encounter will be used for matching on data type constructors, prisms can also encode more complex and abstract patterns.
-- Unlike regular pattern matching if a prism fails to match it won't crash, instead the prism simply won't focus anything.

-- The `_Cons` prism handles the common task of peeling an element off the top of a list-like type.

-- e359
e359 ∷ Maybe (Integer, [Integer])
e359 = [1, 2, 3] ^? _Cons

-- |
-- >>> e359
-- Just (1,[2,3])

-- e360
e360 ∷ Maybe (Char, String)
e360 = "Freedom!" ^? _Cons

-- |
-- >>> e360
-- Just ('F',"reedom!")

-- e361
e361 ∷ Maybe (Char, String)
e361 = "" ^? _Cons

-- |
-- >>> e361
-- Nothing

-- e362
e362 ∷ String
e362 = "Freedom!" & _Cons . _2 %~ reverse

-- |
-- >>> e362
-- "F!modeer"

-- Since `_Cons` is a prism we can run it backwards using. This operation will never fail.
e363 ∷ String
e363 = _Cons # ('F', "reedom")

-- |
-- >>> e363
-- "Freedom"

-- e364
e364 ∷ String
e364 = "Freedom" & _tail %~ reverse

-- |
-- >>> e364
-- "Fmodeer"

-- e365
e365 ∷ String
e365 = "Hello" & _head .~ 'J'

-- |
-- >>> e365
-- "Jello"

-- does not fail
e366 ∷ String
e366 = "" & _head .~ 'J'

-- |
-- >>> e366
-- ""

-- e367
e367 ∷ Bool
e367 = isn't _Empty []

-- |
-- >>> e367
-- False

-- e368
e368 ∷ Bool
e368 = isn't _Empty [1, 2, 3]

-- |
-- >>> e368
-- True

-- The phrasing for 'has' isn't quite as clear.
-- Feel free to simply define 'is = has' if that helps!!!
e369 ∷ Bool
e369 = has _Empty M.empty

-- |
-- >>> e369
-- True

-- e370
e370 ∷ Bool
e370 = has _Empty (S.fromList [1, 2, 3])

-- |
-- >>> e370
-- False

-- The `_Show` prism can Read or Show values to and from their String representations.
-- The "pattern" we're matching on is whether the value can successfully be parsed into the result type (which will be determined by type inference).
-- If the string fails to parse into the output type properly the prism will not match.
-- To run it in reverse it calls "show" on the provided value to turn it back into a string.

-- _Show ∷ (Read a, Show a) ⇒ Prism' String a

-- e371
e371 ∷ Maybe Int
e371 = "12" ^? _Show ∷ Maybe Int

-- |
-- >>> e371
-- Just 12

-- e372
e372 ∷ Maybe Bool
e372 = "12" ^? _Show ∷ Maybe Bool

-- |
-- >>> e372
-- Nothing

-- e373
e373 ∷ Maybe Bool
e373 = "True" ^? _Show ∷ Maybe Bool

-- |
-- >>> e373
-- Just True

-- e374
e374 ∷ Maybe Int
e374 = "apple pie" ^? _Show ∷ Maybe Int

-- |
-- >>> e374
-- Nothing

-- Get a list of all Integers in a sentence!
e375 ∷ [Int]
e375 = "It's True that I ate 3 apples and 5 oranges" ^.. worded . _Show ∷ [Int]

-- |
-- >>> e375
-- [3,5]

-- e376
e376 ∷ [Bool]
e376 = "It's True that I ate 3 apples and 5 oranges" ^.. worded . _Show ∷ [Bool]

-- |
-- >>> e376
-- [True]

------------------------
-- Exercises - Prisms --
------------------------

-- 1. Which prisms will be generated from the following data declaration? Give their names and types!

data ContactInfo where
  CIEmail ∷ String → ContactInfo
  CITelephone ∷ Int → ContactInfo
  CIAddress ∷ String → String → String → ContactInfo

makePrisms ''ContactInfo

-- _CIEmail     ∷ Prism' ContactInfo String
-- _CITelephone ∷ Prism' ContactInfo Int
-- _CIAddress   ∷ Prism' ContactInfo (String, String, String)

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

-- e377
e377 ∷ Bool
e377 = isn't (_Show ∷ Prism' String Bool) "not an int"

-- |
-- >>> e377
-- True

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

-- |
-- >>> _Cons # ('x', "yz") & _tail %~ reverse
-- "xzy"

-- c)
-- input  = "do the hokey pokey"
-- output = Left (Just (Right "do the hokey pokey"))

-- |
-- >>> _Left . _Just . _Right # "do the hokey pokey"
-- Left (Just (Right "do the hokey pokey"))

-------------------------------
-- 9.2 Writing Custom Prisms --
-------------------------------

-- We can build a prism for any pattern which we can reverse.
-- We can imagine a prism as "refracting" your data, splitting it up and focusing only the pieces which match the pattern, dissipating the rest.
-- Luckily the lens library provides us with some helpers to make constructing prisms easy and fool-proof.

-- >>> :info prism
-- prism ∷ (b → t) → (s → Either t a) → Prism s t a b

-- >>> :info prism'
-- prism' ∷ (b → s) → (s → Maybe a) → Prism s s a b

--         EMBEDDING FUNCTION (review aka (#))
--             |
--             |     MATCHING FUNCTION (preview aka (^?))
--             |             |
-- prism  ∷ (b → t) → (s → Either t a) → Prism s t a b   -- fully polymorphic version
-- prism' ∷ (b → s) → (s → Maybe a)    → Prism s s a b   -- simple version (output structure = input structure)

-- The EMBEDDING FUNCTION is where you specify how to reverse your pattern match by embedding the prism's focus back into the pattern.
-- This is the function which will be called when reviewing and it must never fail.

-- The MATCHING FUNCTION determines whether the prism matches or fails.
-- For polymorphic prisms where `s` is not equal to `t` the match function must return a member of the type which matches the new type `t`.
-- This allows type changing traversals to work even when the prism fails to match.
-- For simpler prisms it's sufficient to provide Just the focus, or Nothing if the match fails.

-----------------------------------
-- Rebuilding _Just and _Nothing --
-----------------------------------

_Just' ∷ Prism (Maybe a) (Maybe b) a b
_Just' = prism embed match
  where
    embed ∷ b → Maybe b
    embed = Just
    match ∷ Maybe a → Either (Maybe b) a
    match (Just a) = Right a
    match Nothing = Left Nothing

-- >>> :info Prism'
-- type Prism' s a = Prism s s a a

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

-- e378
e378 ∷ Maybe Int
e378 = 15 ^? _Factor 3 . _Factor 5

-- |
-- >>> e378
-- Just 1

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

-- >>> [prismFizzBuzz n | n ← [1..20]]
-- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz"]

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

-- >>> :info prism
-- prism ∷ (b → t) → (s → Either t a) → Prism s t a b

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

-- >>> :info prism'
-- prism' ∷ (b → s) → (s → Maybe a) → Prism s s a b

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

------------------------------------------------
-- Law Two: Prism Complement (Preview-Review) --
------------------------------------------------

-- This law is effectively the inverse of the previous law.
-- First match then embed!

-- review p (preview p s) == Just s
-- p # (s ^? p) = Just s

-- If we preview with a matching prism to get the focus 'a' ...
-- let Just a = preview myPrism s

-- and then 'review' that focus 'a' ...
-- let s' = review myPrism a

-- then we must end up back where we started.
-- s == s'

-- e379
e379 ∷ Bool
e379 =
  let s = Right 32 ∷ Either () Int
      Just a = preview _Right s
   in review _Right a == s

-- |
-- >>> e379
-- True

-- this doesn't work
-- e379' ∷ Bool
-- e379' =
--   let p = _Right
--       s = Right 32 ∷ Either () Int
--       Just a = preview p s
--    in review p a == s

-- |
-- >>> let s = Right 32
-- >>> let Just a = preview _Right s
-- >>> let s' = review _Right a
-- >>> s == s'
-- True

-- `_Show` is unlawful. (Spaces cause trouble.)
e380 ∷ Bool
e380 =
  let s = "[1, 2, 3]"
      Just a = preview (_Show ∷ Prism' String [Int]) s
      s' = review _Show a
   in s == s'

-- |
-- >>> e380
-- False

-- |
-- Note the different spacing.
-- _Show has normalized the value to have no spaces between values.
-- >>> preview (_Show ∷ Prism' String [Int]) "[1, 2, 3]"
-- Just [1,2,3]

-- In practice we don't care. The two values are equal with respect to their semantic meaning.

---------------------------------------
-- Law Three: Pass-through Reversion --
---------------------------------------

-- Prisms also allow us to change the type parameter (s → t) in the case when we DON'T match!

-- `matching` combinator
-- matching ∷ Prism s t a b → s → Either t a

-- Law #3
--
-- If the prism fails to match and we type cast the structure into a new type
-- we can use the same prism to type cast it back into its original type.
--
-- let Left t  = matching p s
-- let Left s' = matching p t
-- s == s'

-- e381
e381 ∷ Bool
e381 =
  let s = Nothing ∷ Maybe Int
      Left (t ∷ Maybe String) = matching _Just s
      Left (s' ∷ Maybe Int) = matching _Just t
   in s == s'

-- |
-- >>> e381
-- True

-- e382
e382 ∷ Bool
e382 =
  let s = Left "Yo" ∷ Either String Int
      Left (t ∷ Either String Bool) = matching _Right s
      Left (s' ∷ Either String Int) = matching _Right t
   in s == s'

-- |
-- >>> e382
-- True

-- e383
e383 ∷ Bool
e383 =
  let s = Left "Yo" ∷ Either String Int
      Left (t ∷ Either String ()) = matching _Right s
      Left (s' ∷ Either String Int) = matching _Right t
   in s == s'

-- |
-- >>> e383
-- True

-- Of course you are limited to what you can convert to.
-- In this case it must be something like `Either String a`.
e384 ∷ Bool
e384 =
  let s = Left "Yo" ∷ Either String Int
      Left (t ∷ _) = matching _Right s
      Left (s' ∷ Either String Int) = matching _Right t
   in s == s'

----------------------------
-- Exercises - Prism Laws --
----------------------------

-- 1. Implement the following prism and determine whether it's lawful:

-- It should match on sets which contain the provided element!
-- Reviewing adds the element to the Set.
-- ⇒ Matching removes the element from the Set.

_Contains ∷ ∀ a. Ord a ⇒ a → Prism' (Set a) (Set a)
_Contains x = prism' embed match
  where
    embed ∷ Set a → Set a
    embed = S.insert x
    match ∷ Set a → Maybe (Set a)
    match s = if x `elem` s then Just (S.delete x s) else Nothing

-- e385
e385 ∷ Maybe (Set Integer)
e385 = S.fromList [1, 2, 3] ^? _Contains 2

-- |
-- >>> e385
-- Just (fromList [1,3])

-- e386
e386 ∷ Maybe (Set Integer)
e386 = S.fromList [1, 2, 3] ^? _Contains 10

-- |
-- >>> e386
-- Nothing

-- e387
e387 ∷ Set Integer
e387 = _Contains 10 # S.fromList [1, 2, 3]

-- |
-- >>> e387
-- fromList [1,2,3,10]

-- e388
e388 ∷ Set Integer
e388 = _Contains 2 # S.fromList [1, 2, 3]

-- |
-- >>> e388
-- fromList [1,2,3]

-- Is it lawful? Why or why not?

-- Law One - Review-Preview (preview p (review p value) == Just value)
e389 ∷ Bool
e389 =
  let set = S.fromList [1, 2, 3]
   in (_Contains 10 # set) ^? _Contains 10 == Just set

-- |
-- >>> e389
-- True

-- Law One - Review-Preview (preview p (review p value) == Just value)
e390 ∷ Bool
e390 =
  let set = S.fromList [1, 2, 3]
   in (_Contains 2 # set) ^? _Contains 2 == Just set

-- |
-- >>> e390
-- False

-- ⇒ unlawful!!!

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
-- >>> (_Singleton # 1) ^? _Singleton == Just 1
-- True

-- |
-- Law One
-- >>> (_Singleton # ()) ^? _Singleton == Just ()
-- True

-- |
-- Law Two
-- >>> let s = [1]
-- >>> let Just a = preview _Singleton s
-- >>> let s' = review _Singleton a
-- >>> s == s'
-- True

-- Law Three
--
-- matching ∷ Prism s t a b → s → Either t a
--
-- let Left t  = matching p s
-- let Left s' = matching p t
-- s == s'
--
-- Here we don't have much choice: t ∷ [Bool]. (No type change possible in this case.)
e391 ∷ Bool
e391 =
  let s = [] ∷ [Bool]
      Left (t ∷ _) = matching _Singleton s
      Left s' = matching _Singleton t
   in s == s'

-- |
-- >>> e391
-- True

-- ⇒ lawful!!!

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

-- e392
e392 ∷ Path
e392 = Post ["posts", "12345"] "My new post" ^. path

-- |
-- >>> e392
-- ["posts","12345"]

-- e393
e393 ∷ Path
e393 = Get ["posts", "12345"] ^. path

-- |
-- >>> e393
-- ["posts","12345"]
e394 ∷ Request
e394 = Get ["posts", "12345"] & path .~ ["hello"]

-- |
-- >>> e394
-- Get ["hello"]

-- e395
e395 ∷ Path
e395 = Delete ["posts", "12345"] ^. path

-- |
-- >>> e395
-- ["posts","12345"]

-- e396
e396 ∷ Request
e396 = Delete ["posts", "12345"] & path .~ ["hello"]

-- |
-- >>> e396
-- Delete ["hello"]

-- default server handler
serveRequest ∷ Request → String
serveRequest _ = "404 Not Found"

-- e397
e397 ∷ String
e397 = serveRequest $ Post ["hello"] "Is anyone there?"

-- |
-- >>> e397
-- "404 Not Found"

-- e398
e398 ∷ String
e398 = serveRequest $ Get ["hello"]

-- |
-- >>> e398
-- "404 Not Found"

-- e399
e399 ∷ String
e399 = serveRequest $ Delete ["user"]

-- |
-- >>> e399
-- "404 Not Found"

--------------------------
-- Path prefix matching --
--------------------------

_PathPrefix ∷ String → Prism' Request Request
_PathPrefix prefix = prism' embed match
  where
    embed ∷ Request → Request
    embed req = req & path %~ (prefix :)
    match ∷ Request → Maybe Request
    match req | has (path . _head . only prefix) req = Just (req & path %~ drop 1)
    match _ = Nothing

-- e400
e400 ∷ Maybe Request
e400 = Get ["users", "all"] ^? _PathPrefix "users"

-- |
-- >>> e400
-- Just (Get ["all"])

-- e401
e401 ∷ Maybe Request
e401 = Delete ["posts", "12345"] ^? _PathPrefix "posts"

-- |
-- >>> e401
-- Just (Delete ["12345"])

-- fails to match when prefixes differ
e402 ∷ Maybe Request
e402 = Get ["other"] ^? _PathPrefix "users"

-- |
-- >>> e402
-- Nothing

-- Can we run it backwards?
e403 ∷ Request
e403 = _PathPrefix "users" # Get ["all"]

-- |
-- >>> e403
-- Get ["users","all"]

-- e404
e404 ∷ Request
e404 = _PathPrefix "posts" # Delete ["12345"]

-- |
-- >>> e404
-- Delete ["posts","12345"]

-- NOTE: We could have implemented _PathPrefix much easier using the existing `prefixed` prism from the Lens library.

-- >>> :type prefixed
-- prefixed ∷ (Prefixed t, Choice p, Applicative f) =>
--      t → p t (f t) → p t (f t)

-- >>> :info prefixed
-- class Prefixed t where
--   prefixed ∷ t → Prism' t t

-- e.g. prefixed ∷ [a] → Prism' [a] [a]

------------------------------------
-- Altering Sub-Sets of Functions --
------------------------------------

-- |
-- >>> tail [1, 2, 3]
-- [2,3]

-- |
-- tail is unsafe
-- >>> tail []
-- *** Exception: Prelude.tail: empty list

-- We can make it safe with `outside`.

-- prism combinator
-- outside ∷ Prism s t a b → Lens (t → r) (s → r) (b → r) (a → r)

-- `outside` lifts a prism so that it matches on the argument of a function and returns a special lens.
-- When applied to a prism, the `outside` combinator returns a lens which focuses the subset of a function
-- which runs when the argument matches the prism, leaving the rest of the function untouched.

-- If the argument for `tail` is the empty list, then change `tail` to the function `const []`.
-- In all other cases leave `tail` alone.
--
-- If `_Empty` then use `const []`.
-- modifying `tail`
safeTail ∷ [a] → [a]
safeTail = tail & outside _Empty .~ const []

-- in this concrete case
-- outside ∷ Lens ([a] → [a]) (() → [a])

-- e405
e405 ∷ [Integer]
e405 = safeTail [1, 2, 3]

-- |
-- >>> e405
-- [2,3]

-- e406
e406 ∷ [a]
e406 = safeTail []

-- |
-- >>> e406
-- []

-- Handlers
userHandler ∷ Request → String
userHandler req = "User handler! Remaining path: " <> intercalate "/" (req ^. path)

postsHandler ∷ Request → String
postsHandler = const "Post Handler" & outside (_PathPrefix "index") .~ const "Post Index"

-- If (_PathPrefix "users") then use userHandler, else if (_PathPrefix "posts") use postsHandler.
server ∷ Request → String
server =
  serveRequest
    & outside (_PathPrefix "users") .~ userHandler
    & outside (_PathPrefix "posts") .~ postsHandler

-- e407
e407 ∷ String
e407 = server $ Get ["users", "12345"]

-- |
-- >>> e407
-- "User handler! Remaining path: 12345"

-- e408
e408 ∷ String
e408 = server $ Delete ["users", "12345", "profile"]

-- |
-- >>> e408
-- "User handler! Remaining path: 12345/profile"

-- runs the default handler
e409 ∷ String
e409 = server $ Post ["admin"] "DROP TABLE users"

-- |
-- >>> e409
-- "404 Not Found"

-- e410
e410 ∷ String
e410 = server $ Get ["posts"]

-- |
-- >>> e410
-- "Post Handler"

-- e411
e411 ∷ String
e411 = server $ Get ["posts", "index"]

-- |
-- >>> e411
-- "Post Index"

-- e412
e412 ∷ String
e412 = server $ Get ["posts", "foo"]

-- |
-- >>> e412
-- "Post Handler"

---------------------------
-- Matching on HTTP Verb --
---------------------------

-- We can access specific fields of our request type in a perfectly type-safe way!
postsHandler' ∷ Request → String
postsHandler' =
  const "404 Not Found"
    & outside _Post .~ (\(_, body) → "Created post with body: " <> body)
    & outside _Get .~ (\path → "Fetching post at path: " <> intercalate "/" path)
    & outside _Delete .~ (\path → "Deleting post at path: " <> intercalate "/" path)

server' ∷ Request → String
server' =
  serveRequest
    & outside (_PathPrefix "users") .~ userHandler
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

-- 1. Prisms are composable: doing multiple pattern matches in standard Haskell requires nesting function calls or case expressions.

-- 2. Prisms interoperate with the rest of optics, providing a lot of flexibility and expressiveness.
--    You can perform pattern matching inside of an optics chain!

-- In general, prisms are nice when you’re not concerned with whether you’ve covered every possible case.

--------------------------------------------------------------------------------------------
--                                      10. Isos                                          --
--------------------------------------------------------------------------------------------

-------------------------------
-- 10.1 Introduction to Isos --
-------------------------------

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

-- Iso is like Lens with Embedding feature (= create values in an optic path).

-- Because Isos are completely reversible they're the strongest/most constrained of all the optics we've seen.
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

-- Generally:
-- to . from == from . to

------------------------
-- 10.2 Building Isos --
------------------------

-- An Iso is an optic which views data after running its transformation on it.
-- We can view, modify or traverse the data in it's altered form!
-- If we modify the data in its altered form, the iso will convert the result back through the iso into the original form.
-- E.g. we can use an iso to edit a String as though it were a Text, and end up with a String again after the modification.

-- We convert the data through the Iso, run the modification, then convert it back.

--         to       from
--          |         |
-- iso ∷ (s → a) → (b → t) → Iso s t a b

-- >>> :info Iso'
-- type Iso' s a = Iso s s a a

-- >>> :info Iso

-- String → Text
packed ∷ Iso' String Text
packed = iso to from
  where
    to ∷ String → Text
    to = T.pack
    from ∷ Text → String
    from = T.unpack

-- Using `packed` views Strings as though they were Text.
e413 ∷ Text
e413 = ("Ay, caramba!" ∷ String) ^. packed

-- |
-- >>> e413
-- "Ay, caramba!"

-- `review`ing an iso runs its inverse (Text → String).
-- Here we use Iso as a Prism. Reviewing an Iso runs its inverse.
-- So we go from Text to String when reviewing `packed`.
e414 ∷ String
e414 = packed # T.pack "Suffering Succotash!"

-- |
-- >>> e414
-- "Suffering Succotash!"

------------------------------------
-- 10.3 Flipping Isos with `from` --
------------------------------------

-- `from` turns an iso backwards:
-- We can use from to flip existing Isos!

-- from ∷ Iso  s t a b → Iso  b a t s
-- from ∷ Iso' s   a   → Iso'   a   s      -- simpler form

-- e415
e415 ∷ Text
e415 = "Good grief" ^. packed

-- |
-- >>> e415
-- "Good grief"

-- e416
e416 ∷ String
e416 = T.pack "Good grief" ^. from packed

-- |
-- >>> e416
-- "Good grief"

-- Using `from` to flip an Iso.
unpacked ∷ Iso' Text String
unpacked = from packed

-- Note: Both `packed` and `unpacked` are available in Data.Text.Lens from the `lens` library.

-----------------------------------------
-- 10.4 Modification under Isomorphism --
-----------------------------------------

-- e417
e417 ∷ String
e417 = "Idol on a pedestal" & packed %~ T.replace (T.pack "Idol") (T.pack "Sand")

-- |
-- >>> e417
-- "Sand on a pedestal"

-- Using `over` instead of `%~` reads nicely.
e418 ∷ String
e418 = over packed (T.replace (T.pack "Idol") (T.pack "Sand")) "Idol on a pedestal"

-- |
-- >>> e418
-- "Sand on a pedestal"

-- composing Isos with other optics
e419 ∷ Text
e419 = T.pack "Lorem ipsum" & from packed . traversed %~ toUpper

-- |
-- >>> e419
-- "LOREM IPSUM"

-- e420
e420 ∷ Text
e420 = T.pack "Lorem ipsum" & unpacked . traversed %~ toUpper

-- |
-- >>> e420
-- "LOREM IPSUM"

------------------------------------
-- 10.5 Varieties of Isomorphisms --
------------------------------------

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

-- e421
e421 ∷ [Integer]
e421 = reverse . reverse $ [1, 2, 3]

-- |
-- >>> e421
-- [1,2,3]

-- e422
e422 = [1, 2, 3] & reversed'' %~ drop 1

-- |
-- >>> e422
-- [1,2]

-- e423
e423 ∷ [Integer]
e423 = [1, 2, 3] & reversed'' %~ take 1

-- |
-- >>> e423
-- [3]

-- e424
e424 ∷ [Integer]
e424 = [1, 2, 3] & reversed'' %~ take 2

-- |
-- >>> e424
-- [2,3]

-- We gain a lot of power by combining isos with all the other combinators we've learned.
e425 ∷ [Integer]
e425 = [1, 2, 3, 4] ^.. reversed'' . takingWhile (> 2) traversed

-- |
-- >>> e425
-- [4,3]
e426 ∷ String
e426 = "Blue suede shoes" & reversed'' . taking 1 worded .~ "gloves"

-- |
-- >>> e426
-- "Blue suede sevolg"

-- e427
e427 ∷ String
e427 = "Blue suede shoes" & reversed'' . taking 1 worded . reversed'' .~ "gloves"

-- |
-- >>> e427
-- "Blue suede gloves"

-- swap values in a tuple
-- swapped ∷ Iso' (a, b) (b, a)

-- `swapped` lets us view a tuple as though the slots were swapped around.
e428 ∷ (String, String)
e428 = ("Fall", "Pride") ^. swapped

-- |
-- >>> e428
-- ("Pride","Fall")

-- The real type of swapped is actually even more general.
-- It's backed by a Swapped typeclass and works on a lot of different Bifunctors.
-- swapped ∷ (Bifunctor p, Swapped p) ⇒ Iso (p a b) (p c d) (p b a) (p d c)

-- e429
e429 ∷ Either String a
e429 = Right "Field" ^. swapped

-- |
-- >>> e429
-- Left "Field"
e430 ∷ Either a String
e430 = Left "Error" ^. swapped

-- |
-- >>> e430
-- Right "Error"

-- flip function arguments, flip is its own inverse
-- flipped ∷ Iso' (a → b → c) (b → a → c)

-- e431
e431 ∷ String
e431 =
  let (++?) = (++) ^. flipped
   in "A" ++? "B"

-- |
-- >>> e431
-- "BA"

-- curry and uncurry function arguments
-- curried ∷   Iso' ((a, b) → c)  (a → b → c)
-- uncurried ∷ Iso'  (a → b → c) ((a, b) → c)

-- e432
e432 ∷ Integer
e432 =
  let addTuple = (+) ^. uncurried
   in addTuple (1, 2)

-- |
-- >>> e432
-- 3

-- e433
e433 ∷ Integer
e433 = 10 ^. negated

-- |
-- >>> e433
-- -10

-- (-30 + 10) * (-1)
-- e434
e434 ∷ Integer
e434 = over negated (+ 10) 30

-- |
-- >>> e434
-- 20

-- e435
e435 ∷ Integer
e435 = 100 ^. adding 50

-- |
-- >>> e435
-- 150

-- e436
e436 ∷ Double
e436 = 100.0 ^. dividing 10

-- |
-- >>> e436
-- 10.0

-- Be careful of division by 0 for 'dividing'.
e437 ∷ Double
e437 = 100.0 ^. dividing 0

-- >>> e437
-- Numeric.Lens.dividing: divisor 0

-- Be careful of multiplying by 0 for 'multiplying'!
-- There is a division by 0 hidden.
e438 ∷ Double
e438 = 100.0 ^. multiplying 0

-- >>> e438
-- Numeric.Lens.multiplying: factor 0

--------------------
-- Composing Isos --
--------------------

-- Isos compose just like any other optic! They compose both the 'forwards' and 'reversed' transformations.

-- e439
e439 ∷ String
e439 = T.pack "Winter is coming" ^. unpacked . reversed

-- |
-- >>> e439
-- "gnimoc si retniW"

-- e440
e440 ∷ Text
e440 = T.pack "Winter is coming" & unpacked . reversed %~ takeWhile (not . isSpace)

-- |
-- e440
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

-- 441
e441 ∷ Double
e441 = 30.0 & dividing 10 . multiplying 2 +~ 1

-- |
-- >>> e441
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

switchCase ∷ Char → Char
switchCase c = if isUpper c then toLower c else toUpper c

-- >>> switchCase <$> "Hi"
-- "hI"

-- e442
e442 ∷ (Integer, String)
e442 = (32, "Hi") & _2 . involuted (fmap switchCase) .~ "hELLO"

-- |
-- >>> e442
-- (32,"Hello")

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

--------------------------
-- 10.6 Projecting Isos --
--------------------------

-- Since they're reversible and guaranteed to succeed, we can lift an iso using a functor.
-- Lift/map iso' to a new Iso.
-- mapping' ∷ Functor f ⇒ Iso' s a → Iso' (f s) (f a)
-- mapping' iso' = iso (fmap $ view iso') (fmap $ review iso')

-- `mapping` let us easily transform nested portions of structures in our optics path as we go along!

yamlList ∷ [String] → String
yamlList xs = "- " <> intercalate "\n- " xs

-- |
-- >>> yamlList ["Milk", "Eggs","Flour"]
-- "- Milk\n- Eggs\n- Flour"

-- e.g. if Functor is List ([]):
--         unpacked ∷ Iso'  Text   String
-- mapping unpacked ∷ Iso' [Text] [String]

shoppingList ∷ [Text]
shoppingList = [T.pack "Milk", T.pack "Eggs", T.pack "Flour"]

-- transforming `shoppingList`: [Text] → [String]
strShoppingList ∷ [String]
strShoppingList = shoppingList ^. mapping unpacked

-- |
-- >>> yamlList strShoppingList
-- "- Milk\n- Eggs\n- Flour"

-- We can do it all in one go!
e443 ∷ String
e443 = [T.pack "Milk", T.pack "Eggs", T.pack "Flour"] ^. mapping unpacked . to yamlList

-- |
-- >>> e443
-- "- Milk\n- Eggs\n- Flour"

-- We can even use `traverseOf_`!
e444 ∷ IO ()
e444 = traverseOf_ (mapping unpacked . to yamlList) putStrLn [T.pack "Milk", T.pack "Eggs", T.pack "Flour"]

-- >>> e444
-- - Milk
-- - Eggs
-- - Flour

-- We can lift Isos through functors, e.g. from `[String] → String` to `[Text] → Text`
-- This applies to contravariant functors, bifunctors and even profunctors too!
-- The functions are called: `contramapping`, `bimapping`, `dimapping`.

-- We can contramap over the input and map over the output using dimapping!
-- We take the argument, map and unpack, then run our function. Lastly we pack the function results.
textToYamlList ∷ [Text] → Text
textToYamlList = yamlList ^. dimapping (mapping unpacked) packed

--                  |                          |             |
--         [String] → String                   |             |
--                  |                   [Text] → [String]    |
--                  |                                        |
--                  |                                   String → Text
--                  |                            |------------|
--                  |                                  |
--                  |-----------------------→ [String] → String (= yamlList)
--                                      |----------------------------|
--                                                    |
--                                             [Text] → Text

-- This is a simple version doing EXACTLY the same. :-)
textToYamlList' ∷ [Text] → Text
textToYamlList' = T.pack . yamlList . fmap T.unpack

--------------------------------
-- Exercises - Projected Isos --
--------------------------------

-- 1. Fill in the blank!

-- e445
e445 ∷ (String, String)
e445 = ("Beauty", "Age") ^. mapping reversed . swapped

-- |
-- >>> e445
-- ("egA","Beauty")

-- |
--     ("Beauty", "Age") ^. mapping reversed . _
-- >>> ("Beauty", "Age") ^. mapping reversed . swapped
-- ("egA","Beauty")

-- e446
e446 ∷ [Bool]
e446 = [True, False, True] ^. mapping (involuted not)

-- |
-- >>> e446
-- [False,True,False]

-- |
--     [True, False, True] ^. mapping (_         not)
-- >>> [True, False, True] ^. mapping (involuted not)
-- [False,True,False]

-- e447
e447 ∷ [Bool]
e447 = [True, False, True] & mapping (involuted not) %~ filter id

-- |
-- >>> e447
-- [False]

-- |
--     [True, False, True] & _                       %~ filter id
-- >>> [True, False, True] & mapping (involuted not) %~ filter id
-- [False]

-- e448
e448 ∷ String
e448 = (show ^. mapping reversed) 1234

-- |
-- >>> e448
-- "4321"

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

----------------------------
-- 10.7 Isos and Newtypes --
----------------------------

------------------------
-- Coercing with Isos --
------------------------

-- We know about newtypes that the newtype is exactly equivalent to the underlying representation.
-- This means we have a trivial isomorphism between any type and any newtypes over that type.

-- :type coerce
-- coerce ∷ Coercible a b ⇒ a → b

newtype Email = Email String
  deriving (Show)

newtype UserID = UserID String
  deriving (Show)

-- Each of these types are representationally equal to Strings. The only difference is the type!
-- We can use `coerce` to convert between them.

-- Coercible is a special constraint, it's implemented for us by GHC for any newtype, we can use it even though it won't show up in instance lists.

-- >>> :info Coercible
-- {-
-- Coercible is a special constraint with custom solving rules.
-- It is not a class.
-- Please see section `The Coercible constraint`
-- of the user's guide for details.
-- -}
-- type role Coercible representational representational
-- type Coercible ∷ ∀ k. k → k → Constraint
-- class Coercible a b ⇒ Coercible a b

-- only `Show` instance shows up
-- >>> :info Email
-- newtype Email = Email String
-- instance Show Email

-- We need to specify which type to coerce into.
e449 ∷ Email
e449 = coerce "joe@example.com"

-- |
-- >>> e449
-- Email "joe@example.com"

-- If two types are representationally equal we can even skip the middle-man and go directly between two newtypes.
e450 ∷ UserID
e450 = coerce (Email "joe@example.com")

-- |
-- >>> e450
-- UserID "joe@example.com"

-- The `lens` library provides a handy Iso for converting between newtypes.
-- >>> :info coerced
-- coerced ∷ (Coercible s a, Coercible t b) ⇒ Iso s t a b

-- type annotation necessary because `coerced` can go between MANY different types
e451 ∷ Email
e451 = over coerced (reverse ∷ String → String) (Email "joe@example.com")

-- |
-- >>> e451
-- Email "moc.elpmaxe@eoj"

-- we almost always need to add a lot of extra annotations
e452 ∷ Email
e452 = Email "joe@example.com" & (coerced ∷ Iso' Email String) . traversed %~ toUpper

-- |
-- >>> e452
-- Email "JOE@EXAMPLE.COM"

-- using a little helper to avoid type annotations
email ∷ Iso' Email String
email = coerced

-- looks nicer (at the call site)
e453 ∷ Email
e453 = Email "joe@example.com" & email . traversed %~ toUpper

-- |
-- >>> e453
-- Email "JOE@EXAMPLE.COM"

-- `makeLenses` derives exactly this Iso if we define our newtype using record syntax
newtype Mail = Mail {_mail ∷ String}
  deriving (Show)

makeLenses ''Mail

-- e454
e454 ∷ Mail
e454 = Mail "joe@example.com" & mail . traversed %~ toUpper

-- |
-- >>> e454
-- Mail {_mail = "JOE@EXAMPLE.COM"}

--------------------------
-- Newtype Wrapper Isos --
--------------------------

-- `lens` provides the following isos which are a more restricted form of coerced:
--
-- _Wrapped'   ∷ Wrapped s ⇒ Iso' s (Unwrapped s)
-- _Unwrapped' ∷ Wrapped s ⇒ Iso' (Unwrapped s) s

-- These isos are essentially restricted forms of coerced which only map between newtypes and their unwrapped form.
-- They won't transitively map directly between different wrappers.
-- This means they won't map e.g. between Email and UserID.
-- They also don't allow type-changing transformations.
-- These restrictions mean they tend to result in much better type-inference.

-- `makeWrapped` creates _Wrapped', _Unwrapped' and _Wrapping'.
makeWrapped ''Mail

-- _Wrapped seems to be backwards. We are actually unwrapping!
e455 ∷ Mail
e455 = Mail "joe@example.com" & _Wrapped' %~ reverse

-- |
-- >>> e455
-- Mail {_mail = "moc.elpmaxe@eoj"}

-- Make the unwrapping explicit with `_Wrapping`.
-- Here we unwrap a `Mail` explicitly.
e456 ∷ Mail
e456 = Mail "joe@example.com" & _Wrapping' Mail %~ reverse

-- |
-- >>> e456
-- Mail {_mail = "moc.elpmaxe@eoj"}

-- The author prefers to use an explicit Iso such as the one generated by `makeLenses`.
-- But for types in base this can be convenient, e.g. `_Wrapping' Sum`.

---------------
-- 10.8 Laws --
---------------

-----------------------------------------
-- The One and Only Law: Reversibility --
-----------------------------------------

-- Iso law: we can completely reverse the transformation.

-- myIso . from myIso == id
-- from myIso . myIso == id

-- `id` is a valid optic which always focuses its whole argument. It is a valid Iso as well!

-- e457
e457 ∷ String
e457 = view (reversed . from reversed) "Testing one two three"

-- |
-- >>> e457
-- "Testing one two three"

-- e458
e458 ∷ String
e458 = view (from reversed . reversed) "Testing one two three"

-- |
-- >>> e458
-- "Testing one two three"

-- e459
e459 ∷ Integer
e459 = view (negated . from negated) 23

-- |
-- >>> e459
-- 23

-- e460
e460 ∷ Integer
e460 = view (from negated . negated) 23

-- |
-- >>> e460
-- 23

-- Composition of Isos is also an Iso.
myIso ∷ Iso' Double Double
myIso = negated . adding 10.0 . multiplying 372.0

-- e461
e461 ∷ Double
e461 = view (myIso . from myIso) 23.0

-- |
-- >>> e461
-- 23.0

-- It's not perfect, but close enough. :-)
e462 ∷ Double
e462 = view (from myIso . myIso) 23.0

-- |
-- >>> e462
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

-- It's lawful!

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
    to = sortOn snd . zip [0 ..]
    from = (snd <$>) . sortOn fst

-- Superficially this looks okay but this is an UNLAWFUIL Iso!

-- |
-- okay
-- >>> view (sorted . from sorted) [1,6,1,4,1,3,2,5]
-- [1,6,1,4,1,3,2,5]

-- |
-- edge case also okay
-- >>> view (sorted . from sorted) []
-- []

-- |
-- this is how `to` works
-- >>> sortOn snd $ zip [0 ..] [1,6,1,4,1,3,2,5]
-- [(0,1),(2,1),(4,1),(6,2),(5,3),(3,4),(7,5),(1,6)]

-- NOT okay! For certain types of lists the `to` function renumbers the list entries.
e463 ∷ [(Int, Char)]
e463 = [(9, 'a'), (8, 'b'), (7, 'c')] ^. from sorted . sorted

-- |
-- >>> e463
-- [(2,'a'),(1,'b'),(0,'c')]

---------------------------------------------------------------------------------------------
--                                  11. Indexed Optics                                     --
---------------------------------------------------------------------------------------------

-----------------------------------
-- 11.1 What are Indexed Optics? --
-----------------------------------

-- Indexed optics allow you to accumulate information about the current focus as you dive deeper into an optics path.
-- This information could be anything that makes sense in the context of the optic.
-- But it's commonly used with indexed structures to indicate the location you’re currently focusing on.

-- |
-- >>> ["Summer", "Fall", "Winter", "Spring"] ^.. traversed
-- ["Summer","Fall","Winter","Spring"]

-- |
-- `itraversed` keeps track of the current
-- We can use it in place of traversed without noticing any difference.
-- >>> ["Summer", "Fall", "Winter", "Spring"] ^.. itraversed
-- ["Summer","Fall","Winter","Spring"]

-- |
-- we already know this
-- toListOf = flipped ^..
-- >>> toListOf itraversed ["Summer", "Fall", "Winter", "Spring"]
-- ["Summer","Fall","Winter","Spring"]

-- The magic happens when we start using indexed actions!
e464 ∷ [(Int, String)]
e464 = itoListOf itraversed ["Summer", "Fall", "Winter", "Spring"]

-- |
-- >>> e464
-- [(0,"Summer"),(1,"Fall"),(2,"Winter"),(3,"Spring")]

-- the same using infix operator
e465 ∷ [(Int, String)]
e465 = ["Summer", "Fall", "Winter", "Spring"] ^@.. itraversed

-- |
-- >>> e465
-- [(0,"Summer"),(1,"Fall"),(2,"Winter"),(3,"Spring")]

-- It's the action which adds the index to the result!
-- The index isn't part of the focus of any optics in the path!
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

----------
-- Maps --
----------

-- e466
e466 ∷ [String]
e466 = M.fromList [("Monday", "Shopping"), ("Tuesday", "Swimming")] ^.. traversed

-- |
-- >>> e466
-- ["Shopping","Swimming"]

-- The index type of maps is the key, so we can get a list of all elements and their key.
e467 ∷ [(String, String)]
e467 =
  let agenda = M.fromList [("Monday", "Shopping"), ("Tuesday", "Swimming")]
   in agenda ^@.. itraversed

-- |
-- >>> e467
-- [("Monday","Shopping"),("Tuesday","Swimming")]

------------
-- Tuples --
------------

-- e468
e468 ∷ [String]
e468 = (True, "value") ^.. traversed

-- |
-- >>> e468
-- ["value"]

-- e469
e469 ∷ [(Bool, String)]
e469 = (True, "value") ^@.. itraversed

-- >>> e469
-- [(True,"value")]

-----------
-- Trees --
-----------

-- e471
e471 ∷ [String]
e471 =
  let tree = Node "top" [Node "left" [], Node "right" []]
   in tree ^.. traversed

-- |
-- >>> e471
-- ["top","left","right"]

-- e472
e472 ∷ [([Int], String)]
e472 =
  let tree = Node "top" [Node "left" [], Node "right" []]
   in tree ^@.. itraversed

-- |
-- >>> e472
-- [([],"top"),([0],"left"),([1],"right")]

----------------------------
-- 11.2 Index Composition --
----------------------------

-- Indexes can compose alongside the optics, but it can be a bit less intuitive.
-- By default, the index of a path will be the index of the last optic in the path.

-- e473
e473 ∷ [(Int, String)]
e473 =
  let agenda = M.fromList [("Monday", ["Shopping", "Yoga"]), ("Saturday", ["Brunch", "Food coma"])]
   in agenda ^@.. itraversed . itraversed

-- |
-- >>> e473
-- [(0,"Shopping"),(1,"Yoga"),(0,"Brunch"),(1,"Food coma")]

-- the same - the action drive the optic
e474 ∷ [(Int, String)]
e474 =
  let agenda = M.fromList [("Monday", ["Shopping", "Yoga"]), ("Saturday", ["Brunch", "Food coma"])]
   in agenda ^@.. traversed . traversed

-- |
-- >>> e474
-- [(0,"Shopping"),(1,"Yoga"),(0,"Brunch"),(1,"Food coma")]

-- |
-- This won't work though!
-- e475 = let agenda = M.fromList [("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"])]
--          in agenda ^@.. itraversed . traverse

-- index-aware composition operators:
-- (<.)  → Use the index of the optic to the left.
-- (.>)  → Use the index of the optic to the right. This is how (.) already behaves - the default.
-- (<.>) → Combine the indices of both sides as a tuple.

-- e476
e476 ∷ [(String, String)]
e476 =
  let agenda = M.fromList [("Monday", ["Shopping", "Yoga"]), ("Saturday", ["Brunch", "Food coma"])]
   in agenda ^@.. itraversed <. itraversed

-- |
-- >>> e476
-- [("Monday","Shopping"),("Monday","Yoga"),("Saturday","Brunch"),("Saturday","Food coma")]

-- Tuples start nesting.
--
--    collect all indices in a tuple
--             |
--       |-----------|
e477 ∷ [((String, Int), String)]
e477 =
  let agenda = M.fromList [("Monday", ["Shopping", "Yoga"]), ("Saturday", ["Brunch", "Food coma"])]
   in agenda ^@.. itraversed <.> itraversed

-- |
-- >>> e477
-- [(("Monday",0),"Shopping"),(("Monday",1),"Yoga"),(("Saturday",0),"Brunch"),(("Saturday",1),"Food coma")]

-- Unlike normal (.), (<.>) is NOT associative, re-associating will change the way the tuples nest.

-- using an indexed traversal over the characters of each activity name
e478 ∷ [((String, (Int, Int)), Char)]
e478 =
  let agenda = M.fromList [("Monday", ["Shopping", "Yoga"]), ("Saturday", ["Brunch", "Food coma"])]
   in agenda ^@.. itraversed <.> itraversed <.> itraversed

-- |
-- >>> e478
-- [(("Monday",(0,0)),'S'),(("Monday",(0,1)),'h'),(("Monday",(0,2)),'o'),(("Monday",(0,3)),'p'),(("Monday",(0,4)),'p'),(("Monday",(0,5)),'i'),(("Monday",(0,6)),'n'),(("Monday",(0,7)),'g'),(("Monday",(1,0)),'Y'),(("Monday",(1,1)),'o'),(("Monday",(1,2)),'g'),(("Monday",(1,3)),'a'),(("Saturday",(0,0)),'B'),(("Saturday",(0,1)),'r'),(("Saturday",(0,2)),'u'),(("Saturday",(0,3)),'n'),(("Saturday",(0,4)),'c'),(("Saturday",(0,5)),'h'),(("Saturday",(1,0)),'F'),(("Saturday",(1,1)),'o'),(("Saturday",(1,2)),'o'),(("Saturday",(1,3)),'d'),(("Saturday",(1,4)),' '),(("Saturday",(1,5)),'c'),(("Saturday",(1,6)),'o'),(("Saturday",(1,7)),'m'),(("Saturday",(1,8)),'a')]

-- e479
e479 ∷ [(Int, Char)]
e479 =
  let agenda = M.fromList [("Monday", ["Shopping", "Yoga"]), ("Saturday", ["Brunch", "Food coma"])]
   in agenda ^@.. itraversed . itraversed . itraversed

-- |
-- >>> e479
-- [(0,'S'),(1,'h'),(2,'o'),(3,'p'),(4,'p'),(5,'i'),(6,'n'),(7,'g'),(0,'Y'),(1,'o'),(2,'g'),(3,'a'),(0,'B'),(1,'r'),(2,'u'),(3,'n'),(4,'c'),(5,'h'),(0,'F'),(1,'o'),(2,'o'),(3,'d'),(4,' '),(5,'c'),(6,'o'),(7,'m'),(8,'a')]

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

showDayAndNumber ∷ String → Int → String
showDayAndNumber a b = a <> ": " <> show b

-- e480
e480 ∷ [(String, String)]
e480 =
  let agenda = M.fromList [("Monday", ["Shopping", "Yoga"]), ("Saturday", ["Brunch", "Food coma"])]
   in agenda ^@.. icompose showDayAndNumber itraversed itraversed

-- |
-- >>> e480
-- [("Monday: 0","Shopping"),("Monday: 1","Yoga"),("Saturday: 0","Brunch"),("Saturday: 1","Food coma")]

-- This can be a bit clunky, a custom operator might help.
-- (.++) ∷ (Indexed String s t → r) → (Indexed String a b → s → t) → Indexed String a b → r
(.++) = icompose (\a b → a ++ ", " ++ b)

infixl 9 .++

-- The hardest part about writing these custom operators is figuring out their type!
-- You can leave the type signature off and simply use the operator in an expression, then ask GHCi or the language server for the type!

populationMap ∷ Map String (Map String Int)
populationMap = M.fromList [("Canada", M.fromList [("Ottawa", 994837), ("Toronto", 2930000)]), ("Germany", M.fromList [("Berlin", 3748000), ("Munich", 1456000)])]

-- e481
e481 ∷ [(String, Int)]
e481 = populationMap ^@.. itraversed .++ itraversed

-- |
-- >>> e481
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

-- e482
e482 ∷ IO ()
e482 = itraverseOf_ itraversed (\i s → putStrLn (replicate i ' ' <> s)) ["one", "two", "three"]

-- |
-- _            itraversed (\i s → putStrLn (replicate i ' ' <> s)) ["one", "two", "three"]
-- itraverseOf_ itraversed (\i s → putStrLn (replicate i ' ' <> s)) ["one", "two", "three"]

-- Check this in the repl!
-- one
--  two
--   three

-- e483
e483 ∷ IO ()
e483 = itraverseOf_ itraversed (\n s → putStrLn (show n <> ": " <> s)) ["Go shopping", "Eat lunch", "Take a nap"]

-- |
-- itraverseOf_ itraversed (\n s → putStrLn _                    ) ["Go shopping", "Eat lunch", "Take a nap"]
-- itraverseOf_ itraversed (\n s → putStrLn (show n <> ": " <> s)) ["Go shopping", "Eat lunch", "Take a nap"]

-- Check this in the repl!
-- 0: Go shopping
-- 1: Eat lunch
-- 2: Take a nap

-----------------------------
-- 11.3 Filtering by Index --
-----------------------------

-- We can use `indices` to narrow down the focuses using a predicate on the index of our fold or traversal!
-- You simply give it a predicate to run on the index and it'll ignore any focuses that don't match.

-- >>> :info indices

-- get list elements with an 'even' list index
e484 ∷ String
e484 = ['a' .. 'z'] ^.. itraversed . indices even

-- |
-- >>> e484
-- "acegikmoqsuwy"

-- e485
e485 ∷ [Integer]
e485 =
  let ratings = M.fromList [("Dark Knight", 94), ("Dark Knight Rises", 87), ("Death of Superman", 92)]
   in ratings ^.. itraversed . indices (has (prefixed "Dark"))

-- |
-- >>> e485
-- [94,87]

-- If we want to be more specific we can target an exact index using the `index` filter.
-- `index` works similarly to `indices`, but ignores any focus that doesn't have the exact index you specify.

-- e486
e486 ∷ Maybe Char
e486 = ['a' .. 'z'] ^? itraversed . index 10

-- |
-- >>> e486
-- Just 'k'

-- e487
e487 ∷ Maybe Integer
e487 =
  let ratings = M.fromList [("Dark Knight", 94), ("Dark Knight Rises", 87), ("Death of Superman", 92)]
   in ratings ^? itraversed . index "Death of Superman"

-- |
-- >>> e487
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

-- Usually you should prefer the `ix` traversal when working with data structures.
-- But when combining many indexed optics sometimes it's necessary to use `indices` or `index`.

-- 2. Given the following board:

board ∷ [[Char]]
board =
  [ "XOO",
    ".XO",
    "X.."
  ]

-- a) Generate a list of positions alongside their (row, column) coordinates.

-- |
-- >>> board ^@.. itraversed <.> itraversed
-- [((0,0),'X'),((0,1),'O'),((0,2),'O'),((1,0),'.'),((1,1),'X'),((1,2),'O'),((2,0),'X'),((2,1),'.'),((2,2),'.')]

-- b) Set the empty square at (1, 0) to an 'X'.
--    HINT: When using the custom composition operators you'll often need to introduce parenthesis to get the right precedence.

-- |
-- >>> board & (itraversed <.> itraversed) . index (1, 0) .~ 'X'
-- ["XOO","XXO","X.."]

-- c) Get the 2nd column as a list (e.g. "OX."). Try to do it using `index` instead of `indices`!

-- |
-- >>> board ^.. traversed . itraversed
-- "XOO.XOX.."

-- |
-- >>> board ^.. traversed . itraversed . index 1
-- "OX."

-- d) Get the 3rd row as a list. Try to do it using `index` instead of `indices`!
--    HINT: The precedence for this one can be tricky too.

-- |
-- >>> board ^.. (itraversed <. traverse) . index 2
-- "X.."

--------------------------------
-- 11.4 Custom Indexed Optics --
--------------------------------

-- TODO

{- ORMOLU_DISABLE -}

data Board a where
  Board ∷ a → a → a →
          a → a → a →
          a → a → a → Board a
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
-- So to build an IndexedFold we use `ifolding`.

-- >>> :info ifolding
-- ifolding ∷ (Foldable f, Indexable i p, Contravariant g, Applicative g) =>
--   (s → f (i, a)) → Over p g s t a b

-- simplified signature
-- ifolding ∷ Foldable f ⇒ (s → f (i, a)) → IndexedFold i s a

-- We simply have to project our focuses from the structure into a foldable container (e.g. a list).
-- The difference from `folding` is that we also provide an index of type `i` alongside each value.

-- Use a list comprehension to get the list of all coordinate pairs in the correct order.
-- Then zip them with all the slots in our board.
slotsFold ∷ IndexedFold (Position, Position) (Board a) a
slotsFold = ifolding $ zip [(x, y) | y ← [I, II, III], x ← [I, II, III]] . toList

-- column-first ordering
e488 ∷ [(Position, Position)]
e488 = [(x, y) | y ← [I, II, III], x ← [I, II, III]]

-- |
-- >>> e488
-- [(I,I),(II,I),(III,I),(I,II),(II,II),(III,II),(I,III),(II,III),(III,III)]

-- e489
e489 ∷ [((Position, Position), Char)]
e489 = testBoard ^@.. slotsFold

-- |
-- >>> e489
-- [((I,I),'X'),((II,I),'O'),((III,I),'X'),((I,II),'.'),((II,II),'X'),((III,II),'O'),((I,III),'.'),((II,III),'O'),((III,III),'X')]

-- filter indices where the Y coord is 'II' → row
e490 ∷ [((Position, Position), Char)]
e490 = testBoard ^@.. slotsFold . indices ((== II) . snd)

-- |
-- >>> e490
-- [((I,II),'.'),((II,II),'X'),((III,II),'O')]

-- filter indices where the X coord is 'II' → column
e491 ∷ [((Position, Position), Char)]
e491 = testBoard ^@.. slotsFold . indices ((== II) . fst)

-- |
-- >>> e491
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

-- every traversal is also a Fold
e492 ∷ [((Position, Position), Char)]
e492 = testBoard ^@.. slotsTraversal

-- |
-- >>> e492
-- [((I,I),'X'),((II,I),'O'),((III,I),'X'),((I,II),'.'),((II,II),'X'),((III,II),'O'),((I,III),'.'),((II,III),'O'),((III,III),'X')]

-- With Traversals we can also set and modify slots.
-- Setting the whole second row to 'O'.
e493 ∷ Board Char
e493 = testBoard & slotsTraversal . indices ((== II) . snd) .~ 'O'

-- |
-- >>> e493
-- Board 'X' 'O' 'X' 'O' 'O' 'O' '.' 'O' 'X'

-- Setting the whole second column to 'O'.
e494 ∷ Board Char
e494 = testBoard & slotsTraversal . indices ((== II) . fst) .~ 'O'

-- |
-- >>> e494
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

-- The `indexing` helper takes a normal optic and simply adds a numeric index alongside its elements.
e495 ∷ [(Int, Char)]
e495 = T.pack "hello" ^@.. indexing each

-- |
-- >>> e495
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

----------------------------------
-- 11.5 Index-Preserving Optics --
----------------------------------

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
-- However, we can turn `_1` into an index preserving lens with e.g. `cloneIndexPreservingLens` !
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
--                             12. Dealing with Type Errors                               --
--------------------------------------------------------------------------------------------

-- You can skip this chapter.

---------------------------------------------
-- 12.1 Interpreting Expanded Optics Types --
---------------------------------------------

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
  Weather ∷ {_temp ∷ Float,
             _pressure ∷ Float} → Weather
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

-----------------------
-- 14. Classy Lenses --
-----------------------

----------------------------------------------------------
-- 14.1 What are Classy Lenses and When Do I Need Them? --
----------------------------------------------------------

-- Classy lenses aren't really a new type of optic.
-- They are a DESIGN PATTERN solving the following issues:
--
-- • Duplicate Record Fields
-- • Separating Logic
-- • Granular Dependencies

--------------------------------
-- No Duplicate Record Fields --
--------------------------------

-- In Haskell we can't have two records with the same field names!
-- Classy lenses provide a solution for this - an imperfect one, though.

-- If we have several records which all have a `name` field, we need to disambiguate the fields.
-- Idiomatically we use the record name as a field prefix.
newtype Persona = Persona
  { _personaName ∷ String
  }
  deriving (Show)

newtype Pet = Pet
  { _petName ∷ String
  }
  deriving (Show)

-- Not only is this annoyingly verbose, but there's a greater problem in Haskell as a whole.
-- We can't write code which is polymorphic over record fields!
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

-- We can use `makeFields` to generate record-polymorphic lenses for all the fields which have been defined in the idiomatic way,
-- i.e. with record name as a field prefix.
--
-- name ∷ Lens' Persona String
makeFields ''Persona
-- name ∷ Lens' Pet String
makeFields ''Pet

-- `makeFields` creates a unified `HasName` class and instances for each of the records:

-- >>> :info HasName
-- class HasName s a | s → a where
--   name ∷ Lens' s a
-- instance HasName Persona String
-- instance HasName Pet String

-- :browse
-- class HasName s a | s → a where
--   name ∷ Lens' s a

-- :info HasName
-- instance HasName Person String
-- instance HasName Pet String

-- unifying our two greeting functions
greetByName ∷ HasName a String ⇒ a → String
greetByName a = "Hello " <> a ^. name <> "!"

-- |
-- >>> greetByName (Persona "Calvin")
-- "Hello Calvin!"

-- |
-- >>> greetByName (Pet "Hobbes")
-- "Hello Hobbes!"

------------------------------------------------------
-- Separating Logic and Minimizing Global Knowledge --
------------------------------------------------------

data DataDBEnv where
  DataDBEnv ∷
    { _portNumber ∷ Int,
      _hostName ∷ String,
      _databaseUrl ∷ String
    } →
    DataDBEnv
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

-- The code is functional, and doesn't have any problems per se.
-- But both `initialize` and `connectDB` depend directly on Env, and presumably so does EVERY action in our entire code-base that pulls config from the environment.
-- This means that we've set a very rigid dependency on our Env type, if that type changes significantly we'll have to fix code across our entire app!

-- type DatabaseUrl = String

-- Change the environment of connectDB to only the piece it needs (the DatabaseUrl).
-- Then we manually run the reader layer with the correct piece in `main`.
connectDB' ∷ (MonadIO m, MonadReader DatabaseUrl m) ⇒ m ()
connectDB' = do
  url ← ask
  liftIO $ putStrLn ("connecting to db at: " <> url)

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
    initialisieren
    connectDatenbank

---------------------------------------------
-- Granular Dependencies with `makeFields` --
---------------------------------------------

data EnvDb where
  EnvDb ∷
    { _envDbPNumber ∷ Int,
      _envDbHName ∷ String,
      _envDbDatenbankUrl ∷ DatabaseUrl
    } →
    EnvDb
  deriving (Show)

-- Because we imported the DB module, `makeFields` won't define a new HasDatenbankUrl class from the `_envDbDatenbankUrl` field.
makeFields ''EnvDb

--------------------------------
-- Field Requirements Compose --
--------------------------------

-- See app/Init.hs

---------------------------------------
-- 14.2 `makeFields` vs `makeClassy` --
---------------------------------------

data People where
  People ∷
    { _personName ∷ String,
      _favouriteFood ∷ String
    } →
    People
  deriving (Show)

-- makeFieldsNoPrefix ''People
-- generates:
-- class HasPersonName s a | s → a where
--   personName ∷ Lens' s a
-- class HasFavouriteFood s a | s → a where
--   favouriteFood ∷ Lens' s a

makeClassy ''People

-- generates:
-- class HasPeople c where
--   people ∷ Lens' c People
--   favouriteFood ∷ Lens' c String
--   personName ∷ Lens' c String

-- The key difference between these styles is that makeClassy is a bit less granular.
-- It effectively allows you to specify that a type has a Person nested within it somewhere.

-- Refactoring our Database Config example (see app/DBClassy.hs)
newtype EnvClassy = EnvClassy {_envDbConfig ∷ DbConfig} deriving (Show)

-- Instead of using `makeFields` or `makeClassy`, we can go back to our roots with makeLenses.
makeLenses ''EnvClassy

-- Creates the following lens:
-- envDbConfig ∷ Iso' Env DbConfig

-- We have to write an instance for HasDbConfig which specifies where in our record to find the DbConfig.
-- We're just delegating to the lens we generated with `makeLenses`.
instance HasDbConfig EnvClassy where
  dbConfig = envDbConfig

-- |
-- >>> let env = EnvClassy (DbConfig "dbAdmin" 100)
-- >>> env ^. databaseUser
-- "dbAdmin"

-- |
-- >>> let env = EnvClassy (DbConfig "dbAdmin" 100)
-- >>> env ^. maxConnections
-- 100

-- |
-- >>> let env = EnvClassy (DbConfig "dbAdmin" 100)
-- >>> env ^. dbConfig
-- DbConfig {_databaseUser = "dbAdmin", _maxConnections = 100}

-- `makeFields` is more helpful for reducing the annoyances of records with shared names and for implementing field-polymorphic functions.
--  Whereas `makeClassy` tends to scale a little better for large projects.
-- `makeClassy` is less granular, but also requires significantly fewer constraints.
