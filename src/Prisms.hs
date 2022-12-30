-- {-# OPTIONS_GHC -ddump-splices #-}

module Prisms where

import Control.Lens
  ( AsEmpty (_Empty),
    Cons (_Cons),
    Field1 (_1),
    Field2 (_2),
    Lens',
    Prism,
    Prism',
    each,
    folded,
    has,
    isn't,
    lens,
    makePrisms,
    matching,
    only,
    outside,
    preview,
    prism,
    prism',
    review,
    traversed,
    worded,
    (#),
    (%~),
    (&),
    (+~),
    (.~),
    (<>~),
    (^.),
    (^..),
    (^?),
    _Just,
    _Left,
    _Nothing,
    _Right,
    _Show,
    _head,
    _tail,
  )
import Data.Foldable (for_)
import Data.List (intercalate, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

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
-- review = REverse VIEW

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
