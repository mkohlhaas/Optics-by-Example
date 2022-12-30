-- {-# OPTIONS_GHC -ddump-splices #-}

module Isos where

import Control.Lens
  ( Field2 (_2),
    Iso,
    Iso',
    Wrapped (_Wrapped'),
    coerced,
    dimapping,
    enum,
    flipped,
    from,
    involuted,
    iso,
    makeLenses,
    makeWrapped,
    mapping,
    over,
    reversed,
    swapped,
    taking,
    takingWhile,
    to,
    traverseOf_,
    traversed,
    uncurried,
    view,
    worded,
    (#),
    (%~),
    (&),
    (+~),
    (.~),
    (^.),
    (^..),
    _Wrapping',
  )
import Data.Char (isSpace, isUpper, toLower, toUpper)
import Data.Coerce (coerce)
import Data.List (intercalate, sort, sortOn, transpose)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Lens (adding, dividing, multiplying, negated)

--------------------------------------------------------------------------------------------
--                                      10. Isos                                          --
--------------------------------------------------------------------------------------------

-------------------------------
-- 10.1 Introduction to Isos --
-------------------------------

-- An isomorphism is a COMPLETELY REVERSIBLE transformation between two types or formats.

-----------------------------------------
-- How do Isos fit into the Hierarchy? --
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
