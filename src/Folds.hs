-- {-# OPTIONS_GHC -ddump-splices #-}

module Folds where

import Control.Arrow ((>>>))
import Control.Lens
  ( Each (each),
    Field1 (_1),
    Field2 (_2),
    Fold,
    Identity (Identity),
    Lens',
    allOf,
    anyOf,
    backwards,
    both,
    dropping,
    droppingWhile,
    elemOf,
    filtered,
    filteredBy,
    findOf,
    firstOf,
    foldMapByOf,
    foldMapOf,
    folded,
    folding,
    foldOf,
    foldByOf,
    forOf_,
    has,
    hasn't,
    lastOf,
    lengthOf,
    makeLenses,
    maximumOf,
    maximumByOf,
    minimumOf,
    minimumByOf,
    only,
    preview,
    productOf,
    sumOf,
    taking,
    takingWhile,
    to,
    toListOf,
    traverseOf_,
    view,
    worded,
    (^.),
    (^?),
    (^..),
  )
import Control.Monad.State (execState, modify)
import Data.Char (isAlpha, toUpper)
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Dual (Dual), Sum (Sum))
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S

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
