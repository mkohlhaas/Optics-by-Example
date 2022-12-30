-- {-# OPTIONS_GHC -ddump-splices #-}

module Traversals where

import Control.Applicative (Applicative (liftA2), ZipList (ZipList))
import Control.Monad.State (evalState, get, modify)
import Control.Lens
  ( Each (each),
    Field1 (_1),
    Field2 (_2),
    Lens',
    Traversal,
    Traversal',
    beside,
    both,
    dropping,
    element,
    elementOf,
    filtered,
    filteredBy,
    folded,
    lined,
    makeLenses,
    only,
    partsOf,
    sequenceAOf,
    taking,
    takingWhile,
    traverseOf,
    traversed,
    unsafePartsOf,
    worded,
    (%%~),
    (%~),
    (&),
    (+~),
    (*~),
    (.~),
    (^.),
    (^..),
    (^?),
  )
import Data.Char (toLower, toUpper)
import Data.Either.Validation (Validation (..))
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tree (Tree (Node))
import Text.Read (readMaybe)

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
  AUser ::
    { _aname ∷ String,
      _age ∷ Int
    } ->
    AUser
  deriving (Show)

makeLenses ''AUser

data Account where
  Account ::
    { _accId ∷ String,
      _user ∷ AUser
    } ->
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
type UserId = String

data User where
  User ::
    { _firstName ∷ String,
      _lastName ∷ String,
      _userEmail ∷ String
    } ->
    User
  deriving (Show)

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
