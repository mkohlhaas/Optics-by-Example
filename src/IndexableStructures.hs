-- {-# OPTIONS_GHC -ddump-splices #-}

module IndexableStructures where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens
  ( At (..),
    Field1 (_1),
    Field2 (_2),
    Index,
    IxValue,
    Ixed (..),
    Lens',
    both,
    failing,
    failover,
    filtered,
    makeLenses,
    non,
    pre,
    sans,
    traverseOf,
    traversed,
    (%%~),
    (&),
    (*~),
    (+~),
    (.~),
    (?~),
    (^.),
    (^..),
    (^?),
    _head,
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (toLower, toUpper)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Tree (Tree (Node))
import GHC.Word (Word8)

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
