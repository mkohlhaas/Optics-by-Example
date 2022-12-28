-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Lens (asIndex, at, filtered, filteredBy, foldMapOf, has, ix, lengthOf, makeLensesFor, maximumByOf, only, reindexed, selfIndex, sumOf, toListOf, view, withIndex, (#), (%@~), (%~), (&), (*~), (<.), (?~), (^..), (^?), (^@..), _Nothing)
import Data.Aeson (FromJSON, Key, ToJSON, Value)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Lens (key, members, nth, values, _Array, _Bool, _Double, _Integer, _JSON, _JSON', _Number, _Object, _String)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Text (Text, toUpper)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Text.RawString.QQ (r)

main ∷ IO ()
main = putStrLn "Applying Optics to JSON!"

--------------
-- 15. JSON --
--------------

-----------------------------
-- 15.1 Introspecting JSON --
-----------------------------

-- JSON values are effectively untyped, or more accurately they're all part of one BIG type.
-- `aeson` calls this JSON catch-all type Value.

-- >>> :info _String
--   _String ∷ Prism' t Text

-- >>> :info _Bool
--   _Bool ∷ Prism' t Bool
-- ...

-- `lens-aeson` provides prisms for all the major JSON subtypes ("json-like" types are represented as `t`):
-- • _String ∷ Prism' t Text
-- • _Bool   ∷ Prism' t Bool
-- • _Null   ∷ Prism' t ()
-- • _Object ∷ Prism' t (HashMap Text Value)
-- • _Array  ∷ Prism' t (Vector Value)

-- Numeric prisms convert automatically the generic JSON number type:
-- • _Number   ∷              Prism' t Scientific
-- • _Double   ∷              Prism' t Double
-- • _Integer  ∷              Prism' t Integer
-- • _Integral ∷ Integral n ⇒ Prism' t n

-- The prisms will parse values from strings into JSON.
e01 ∷ Maybe Double
e01 = ("42" ∷ String) ^? _Double

-- Need a type annotation for "42"
-- >>> :type "42"
-- "42" ∷ IsString p ⇒ p

-- |
-- >>> e01
-- Just 42.0

-- e02
e02 ∷ Maybe Integer
e02 = ("42" ∷ String) ^? _Integer

-- |
-- >>> e02
-- Just 42

-- JSON strings must be quoted.
e03 ∷ Maybe Text
e03 = ("42" ∷ String) ^? _String

-- >>> :info _String
--   _String ∷ Prism' t Text

-- |
-- >>> e03
-- Nothing

-- JSON strings must be quoted.
e04 ∷ Maybe Text
e04 = ("\"42\"" ∷ String) ^? _String

-- |
-- >>> e04
-- Just "42"

-- JSON strings must be quoted.
e05 ∷ Maybe Text
e05 = ("\"Hello, World!\"" ∷ String) ^? _String

-- |
-- >>> e05
-- Just "Hello, World!"

-- Failure to parse a value fails the prism traversal.
e06 ∷ Maybe Text
e06 = ("{invalid JSON}" ∷ String) ^? _String

-- |
-- >>> e06
-- Nothing

-- If we mismatch the expected type it will fail even if the JSON is a well formed value of a different type.
e07 ∷ Maybe Double
e07 = ("\"Hello, World!\"" ∷ String) ^? _Double

-- |
-- >>> e07
-- Nothing

-- More structured JSON types using the `r` Quasi-Quoter to avoid escaping.
jsonObject ∷ String
jsonObject =
  [r|
      { "name": "Jack Sparrow",
        "rank": "Captain" }
  |]

jsonArray ∷ String
jsonArray =
  [r|
      ["North",
       "East",
       "South",
       "West" ]
  |]

-- e08
e08 ∷ Maybe (KeyMap Value)
e08 = jsonObject ^? _Object

-- |
-- Maybe KeyMap
-- >>> e08
-- Just (fromList [("name",String "Jack Sparrow"),("rank",String "Captain")])

-- e09
e09 ∷ Maybe (Vector Value)
e09 = jsonArray ^? _Array

-- |
-- Maybe Vector
-- >>> e09
-- Just [String "North",String "East",String "South",String "West"]

-- By introducing some helpers (e.g. `key`, `nth` - see below) we don't need to deal with KeyMaps or Vectors directly.

---------------------------------------------
-- 15.2 Diving Deeper into JSON Structures --
---------------------------------------------

blackPearl ∷ String
blackPearl =
  [r|
      { "name": "Black Pearl",
        "crew": [ { "name": "Jack Sparrow",
                    "rank": "Captain" },
                  { "name": "Will Turner",
                    "rank": "First Mate" } ] }
  |]

-- We want to get the first crew member of the ship from its array of crew members.
-- This works, but it's longish and hard to read.
e10 ∷ Maybe Text
e10 = blackPearl ^? _Object . ix "crew" . _Array . ix 0 . _Object . ix "name" . _String

-- |
-- >>> e10
-- Just "Jack Sparrow"

-- Using the helpers `key` and `nth`.
e11 ∷ Maybe Text
e11 = blackPearl ^? key "crew" . nth 0 . key "name" . _String

-- |
-- >>> e11
-- Just "Jack Sparrow"

-- >>> :info key
-- key ∷ AsValue t ⇒ Key → Traversal' t Value

-- definition of key
-- key ∷ Text → Traversal' t Value
-- key k = _Object . ix k

-- >>> :info nth
-- nth ∷ AsValue t ⇒ Int → Traversal' t Value

-- definition of nth
-- nth ∷ Int → Traversal' t Value
-- nth i = _Array . ix i

-- `key` and `nth` are traversals which use `ix`.
-- So they can't insert new values unless there's already a value at the given key.
-- To insert values into a JSON Object you can match on the Object and then use `at` like you would with a Map.
e12 ∷ String
e12 = blackPearl & _Object . at "name" ?~ "Purple Pearl"

-- >>> e12
-- "{\"crew\":[{\"name\":\"Jack Sparrow\",\"rank\":\"Captain\"},{\"name\":\"Will Turner\",\"rank\":\"First Mate\"}],\"name\":\"Purple Pearl\"}"

-- cleaned
-- "{crew: [{name: Jack Sparrow, rank: Captain}, {name: Will Turner, rank: First Mate }], name: Purple Pearl }"

------------------------------------------------------
-- 15.3 Traversing into Multiple JSON Substructures --
------------------------------------------------------

-- Sometimes we need to collect or modify values from many spots within a JSON structure.
-- E.g. we have a list of users and want to collect all of their email addresses.

-----------------------
-- Traversing Arrays --
-----------------------

-- >>> :info values
-- values ∷ AsValue t ⇒ IndexedTraversal' Int t Value

-- traversing arrays with `values`
-- values ∷ AsValue t ⇒ IndexedTraversal' Int t Value
-- values = _Array . traversed

-- `values` allows us to traverse all the values in a Value if it's an Array.
e13 ∷ [Integer]
e13 = ("[1, 2, 3]" ∷ String) ^.. values . _Integer

-- |
-- >>> e13
-- [1,2,3]

-- If any elements of the traversal don't match our assumptions they'll be SILENTLY EXCLUDED!
e14 ∷ [Integer]
e14 = ("[1, null, 2, \"Hi mom!\", 3]" ∷ String) ^.. values . _Integer

-- |
-- >>> e14
-- [1,2,3]

-- If any elements of the traversal don't match our assumptions they'll be SILENTLY EXCLUDED!
e15 ∷ [Text]
e15 = ("[1, null, 2, \"Hi mom!\", 3, \"It's me!\"]" ∷ String) ^.. values . _String

-- |
-- >>> e15
-- ["Hi mom!","It's me!"]

-- `values` is an `IndexedTraversal' Int`.
-- So we have access to the array index even after we've descended into each element.
e16 ∷ [(Int, Text)]
e16 = ("[\"a\", \"b\", \"c\"]" ∷ String) ^@.. values . _String

-- |
-- >>> e16
-- [(0,"a"),(1,"b"),(2,"c")]

-- the same with Integers
e17 ∷ [(Int, Integer)]
e17 = ("[1, 2, 3]" ∷ String) ^@.. values . _Integer

-- |
-- >>> e17
-- [(0,1),(1,2),(2,3)]

-- It includes the correct array index even if we've filtered out values
e18 ∷ [(Int, Text)]
e18 = ("[\"a\", 1, \"b\", null, \"c\", []]" ∷ String) ^@.. values . _String

-- |
-- >>> e18
-- [(0,"a"),(2,"b"),(4,"c")]

-- the same with Integers
e19 ∷ [(Int, Integer)]
e19 = ("[\"a\", 1, \"b\", null, \"c\", [], 2]" ∷ String) ^@.. values . _Integer

-- |
-- >>> e19
-- [(1,1),(6,2)]

-- As long as we're chaining together Traversals and Prisms we can modify our focus.
e20 ∷ String
e20 = ("[\"a\", \"b\", \"c\"]" ∷ String) & values . _String %~ toUpper

-- |
-- >>> e20
-- "[\"A\",\"B\",\"C\"]"

-- the same with Integers
e21 ∷ String
e21 = ("[1, 2, 3]" ∷ String) & values . _Integer %~ (+ 10)

-- |
-- >>> e21
-- "[11,12,13]"

-- a more complex example
fleet ∷ String
fleet =
  [r| [ { "name": "Black Pearl",
          "crew": [ { "name": "Jack Sparrow",
                      "rank": "Captain" },
                    { "name": "Will Turner",
                      "rank": "First Mate" } ] },
        { "name": "Flying Dutchman",
          "crew": [ { "name": "Davy Jones",
                      "rank": "Captain" },
                    { "name": "Bootstrap Bill",
                      "rank": "First Mate" } ] } ] |]

-- e22
e22 ∷ [Value]
e22 = fleet ^.. values

-- |
-- >>> e22
-- [Object (fromList [("crew",Array [Object (fromList [("name",String "Jack Sparrow"),("rank",String "Captain")]),Object (fromList [("name",String "Will Turner"),("rank",String "First Mate")])]),("name",String "Black Pearl")]),Object (fromList [("crew",Array [Object (fromList [("name",String "Davy Jones"),("rank",String "Captain")]),Object (fromList [("name",String "Bootstrap Bill"),("rank",String "First Mate")])]),("name",String "Flying Dutchman")])]

-- all names of every ship in our fleet
e23 ∷ [Text]
e23 = fleet ^.. values . key "name" . _String

-- |
-- >>> e23
-- ["Black Pearl","Flying Dutchman"]

-- names of all crew members in our entire fleet
e24 ∷ [Text]
e24 = fleet ^.. values . key "crew" . values . key "name" . _String

-- |
-- >>> e24
-- ["Jack Sparrow","Will Turner","Davy Jones","Bootstrap Bill"]

-- include the ship each member is from alongside their name
e25 ∷ [(Text, Text)]
e25 = fleet ^@.. values . reindexed (view (key "name" . _String)) selfIndex <. (key "crew" . values . key "name" . _String)

-- |
-- >>> e25
-- [("Black Pearl","Jack Sparrow"),("Black Pearl","Will Turner"),("Flying Dutchman","Davy Jones"),("Flying Dutchman","Bootstrap Bill")]

------------------------
-- Traversing Objects --
------------------------

-- traversing objects with `members`
-- >>> :info members
-- members ∷ AsValue t ⇒ IndexedTraversal' Key t Value

cargo ∷ String
cargo =
  [r| { "emeralds":  327,
        "rubies":    480,
        "sapphires": 621,
        "opals":     92,
        "dubloons":  34 }
  |]

-- count of all the items (ordered by keys)
e26 ∷ [Integer]
e26 = cargo ^.. members . _Integer

-- >>> cargo ^.. members
-- [Number 34.0,Number 327.0,Number 92.0,Number 480.0,Number 621.0]

-- |
-- >>> e26
-- [34,327,92,480,621]

-- sum of all items
e27 ∷ Integer
e27 = sumOf (members . _Integer) cargo

-- |
-- >>> e27
-- 1554

-- getting the item name alongside the quantity
-- `_Integer` is an index-preserving optic so we don't need to muck about with `(<.)`.
e28 ∷ [(Key, Integer)]
e28 = cargo ^@.. members . _Integer

-- |
-- >>> e28
-- [("dubloons",34),("emeralds",327),("opals",92),("rubies",480),("sapphires",621)]

-- item with the most number of items ;-)
-- Unfortunately there's no `imaximumOf` so we'll need to explicitly include the index into the fold using `withIndex`.
e29 ∷ Maybe (Key, Integer)
e29 = maximumByOf (members . _Integer . withIndex) (comparing snd) cargo

-- |
-- >>> e29
-- Just ("sapphires",621)

---------------------------------
-- 15.4 Filtering JSON Queries --
---------------------------------

-- The trick with filtering queries is that we may be filtering on a different aspect than we return from the query.
-- In this case we only want to focus the name of the crew member, but we're filtering on the crew member's rank.
-- This means we need to run our filter in the middle of our path before we've lost access to the information we need.
--
-- ⇒ use `filtered` and `filteredBy`

-- all captains
-- `OverloadedStrings` allows our "Captain" string to be changed into a JSON string Value which is compared to the rank's Value.
e30 ∷ [Text]
e30 = fleet ^.. values . key "crew" . values . filteredBy (key "rank" . only "Captain") . key "name" . _String

-- |
-- >>> e30
-- ["Jack Sparrow","Davy Jones"]

-- all captains
-- explicit conversion in case we are not using OverloadedStrings
e31 ∷ [Text]
e31 = fleet ^.. values . key "crew" . values . filteredBy (key "rank" . _String . only "Captain") . key "name" . _String

-- |
-- >>> e31
-- ["Jack Sparrow","Davy Jones"]

-- all captains
-- using `filtered` instead of `filteredBy`
-- `filteredBy` is newer and now more idiomatic
e32 ∷ [Text]
e32 = fleet ^.. values . key "crew" . values . filtered (has (key "rank" . only "Captain")) . key "name" . _String

-- |
-- >>> e32
-- ["Jack Sparrow","Davy Jones"]

------------------------------------------------------------
-- 15.5 Serializing & Deserializing within an Optics Path --
------------------------------------------------------------

-- In Haskell we would typically prefer to work with strongly typed records rather than nebulously typed Value objects.
-- Let's see how we can reflect values from their Value representations into their structured Haskell forms and back within a Traversal.

-- record type with ToJSON and FromJSON instances
data Creature where
  Creature ∷
    { creatureName ∷ String,
      creatureSize ∷ Double
    } →
    Creature
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

creatureSightings ∷ String
creatureSightings =
  [r| { "Arctic":  [ { "creatureName": "Giant Squid",
                       "creatureSize":  45.2 } ],
        "Pacific": [ { "creatureName": "Kraken",
                       "creatureSize":  124.4 },
                     { "creatureName": "Ogopogo",
                       "creatureSize":  34.6 } ] } |]

-- When defining our Creature type we didn’t prefix our field names with the standard underscores because it would mess with our derived JSON instances.
-- But we'll still want some lenses for interacting with our Creature type.
-- So let's derive some anyways using `makeLensesFor`.

--                field name       lens name
--                     |               |
makeLensesFor [("creatureName", "creatureNameL"), ("creatureSize", "creatureSizeL")] ''Creature

-- The `_JSON'` prism can convert between any JSON-like `t` and any type `a` which can be serialized to or from a JSON value:
-- >>> :info _JSON
-- class AsJSON t where
--   _JSON ∷ (FromJSON a, ToJSON b) ⇒ Prism t t a b

-- Since the result type of _JSON' is polymorphic over anything that implements ToJSON and FromJSON we'll usually need type annotations.
e33 ∷ [Creature]
e33 = creatureSightings ^.. members . values . _JSON'

-- |
-- >>> e33
-- [Creature {creatureName = "Giant Squid", creatureSize = 45.2},Creature {creatureName = "Kraken", creatureSize = 124.4},Creature {creatureName = "Ogopogo", creatureSize = 34.6}]

-- using TypeApplications (in this case for setting values in the JSON blob)
-- @_ ⇒ we don't care what we're converting from
e34 ∷ String
e34 = creatureSightings & members . values . _JSON @_ @Creature . creatureSizeL *~ 1.2

-- |
-- >>> e34
-- "{\"Arctic\":[{\"creatureName\":\"Giant Squid\",\"creatureSize\":54.24}],\"Pacific\":[{\"creatureName\":\"Kraken\",\"creatureSize\":149.28},{\"creatureName\":\"Ogopogo\",\"creatureSize\":41.52}]}"

-- From the use of creatureSizeL GHC can actually infer the type we want to convert to.
-- Error messages could be more useful with type annotations, though.
e35 ∷ String
e35 = creatureSightings & members . values . _JSON . creatureSizeL *~ 1.2

-- |
-- >>> e35
-- "{\"Arctic\":[{\"creatureName\":\"Giant Squid\",\"creatureSize\":54.24}],\"Pacific\":[{\"creatureName\":\"Kraken\",\"creatureSize\":149.28},{\"creatureName\":\"Ogopogo\",\"creatureSize\":41.52}]}"

--------------------------------
-- Exercises - Kubernetes API --
--------------------------------

pods ∷ ByteString
pods =
  [r|
      {
          "kind": "List",
          "apiVersion": "v1",
          "items": [
              {
                "kind": "Pod",
                "apiVersion": "v1",
                "metadata": {
                    "name": "redis-h315w",
                    "creationTimestamp": "2019-03-23T19:42:21Z",
                    "labels": {
                        "name": "redis",
                        "region": "usa" } },
                "spec": {
                    "containers": [
                        {
                          "name": "redis",
                          "image": "redis",
                          "ports": [
                              {
                                "name": "redis",
                                "hostPort": 27017,
                                "containerPort": 27017,
                                "protocol": "TCP"
                              } ],
                          "resources": {
                              "requests": {
                              "cpu": "100m" } } } ] } },
              {
                "kind": "Pod",
                "apiVersion": "v1",
                "metadata": {
                    "name": "web-4c5bj",
                    "creationTimestamp": "2019-02-24T20:23:56Z",
                    "labels": {
                        "name": "web",
                        "region": "usa" } },
                "spec": {
                    "containers": [
                        {
                          "name": "web",
                          "image": "server",
                          "ports": [
                              {
                                "name": "http-server",
                                "containerPort": 3000,
                                "protocol": "TCP"
                              } ],
                          "resources": {
                              "requests": {
                              "cpu": "100m" } } } ] } } ] }
  |]

-- 1. Your first task should be mostly straightforward: get the api version which was used to make the call.

-- api version
e36 ∷ Maybe Text
e36 = pods ^? key "apiVersion" . _String

-- |
-- >>> e36
-- Just "v1"

-- 2. Next, count the number of all containers across all pods. You can assume that every element of "items" is a pod.

-- number of all containers across all pods
e37 ∷ Int
e37 = lengthOf (key "items" . values . key "spec" . key "containers" . values) pods

-- |
-- >>> e37
-- 2

-- 3. Return the "name" (as Text) of all containers which have the same value for their "image" and "name" fields.

-- "name" of all containers which have the same value for their "image" and "name" fields
e38 ∷ [Text]
e38 = toListOf (key "items" . values . key "spec" . key "containers" . values . filtered (\v → v ^? key "name" == v ^? key "image") . key "name" . _String) pods

-- |
-- >>> e38
-- ["redis"]

-- the same (with toListOf operator)
e39 ∷ [Text]
e39 = pods ^.. key "items" . values . key "spec" . key "containers" . values . filtered (\v → v ^? key "name" == v ^? key "image") . key "name" . _String

-- |
-- >>> e39
-- ["redis"]

-- 4. Collect a list of all "containerPort"s alongside their Pod's "metadata > name".

-- "containerPort"s alongside their Pod's "metadata > name"
e40 ∷ [(Text, Integer)]
e40 = toListOf ((key "items" . values . reindexed (view (key "metadata" . key "name" . _String)) selfIndex <. (key "spec" . key "containers" . values . key "ports" . values . key "containerPort" . _Integer)) . withIndex) pods

-- |
-- >>> e40
-- [("redis-h315w",27017),("web-4c5bj",3000)]

-- the same (with toListOf operator)
e41 ∷ [(Text, Integer)]
e41 = pods ^.. (key "items" . values . reindexed (view (key "metadata" . key "name" . _String)) selfIndex <. (key "spec" . key "containers" . values . key "ports" . values . key "containerPort" . _Integer)) . withIndex

-- |
-- >>> e41
-- [("redis-h315w",27017),("web-4c5bj",3000)]

-- 5. Uppercase the label values inside each pod's metadata.

-- uppercasing
e42 ∷ ByteString
e42 = pods & key "items" . values . key "metadata" . key "labels" . members . _String %~ toUpper

-- |
-- >>> e42
-- "{\"apiVersion\":\"v1\",\"items\":[{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-03-23T19:42:21Z\",\"labels\":{\"name\":\"REDIS\",\"region\":\"USA\"},\"name\":\"redis-h315w\"},\"spec\":{\"containers\":[{\"image\":\"redis\",\"name\":\"redis\",\"ports\":[{\"containerPort\":27017,\"hostPort\":27017,\"name\":\"redis\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}},{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-02-24T20:23:56Z\",\"labels\":{\"name\":\"WEB\",\"region\":\"USA\"},\"name\":\"web-4c5bj\"},\"spec\":{\"containers\":[{\"image\":\"server\",\"name\":\"web\",\"ports\":[{\"containerPort\":3000,\"name\":\"http-server\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}}],\"kind\":\"List\"}"

-- 6. Set a resource request of memory: "256M" for every container.

-- setting resource reqest
e43 ∷ ByteString
e43 = pods & key "items" . values . key "spec" . key "containers" . values . key "resources" . key "requests" . _Object . at "memory" ?~ "256M"

-- |
-- >>> e43
-- "{\"apiVersion\":\"v1\",\"items\":[{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-03-23T19:42:21Z\",\"labels\":{\"name\":\"redis\",\"region\":\"usa\"},\"name\":\"redis-h315w\"},\"spec\":{\"containers\":[{\"image\":\"redis\",\"name\":\"redis\",\"ports\":[{\"containerPort\":27017,\"hostPort\":27017,\"name\":\"redis\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\",\"memory\":\"256M\"}}}]}},{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-02-24T20:23:56Z\",\"labels\":{\"name\":\"web\",\"region\":\"usa\"},\"name\":\"web-4c5bj\"},\"spec\":{\"containers\":[{\"image\":\"server\",\"name\":\"web\",\"ports\":[{\"containerPort\":3000,\"name\":\"http-server\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\",\"memory\":\"256M\"}}}]}}],\"kind\":\"List\"}"

-- BONUS Questions

-- These problems require a bit more thought, and will probably require some unorthodox uses of indexes and filtering. Good luck!
-- Don't get too frustrated, they're deliberately very hard!

-- 7. Get a Set of all metadata label keys used in the response.

-- Set of all metadata label keys
e44 ∷ S.Set Key
e44 = foldMapOf (key "items" . values . key "metadata" . key "labels" . members . asIndex) S.singleton pods

-- |
-- >>> e44
-- fromList ["name","region"]

-- 8. Set the hostPort to 8080 on any "port" descriptions where it is unset.

-- setting the hostPort
e45 ∷ ByteString
e45 = pods & key "items" . values . key "spec" . key "containers" . values . key "ports" . values . _Object . at "hostPort" . filteredBy _Nothing ?~ _Number # 8080

-- |
-- >>> e45
-- "{\"apiVersion\":\"v1\",\"items\":[{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-03-23T19:42:21Z\",\"labels\":{\"name\":\"redis\",\"region\":\"usa\"},\"name\":\"redis-h315w\"},\"spec\":{\"containers\":[{\"image\":\"redis\",\"name\":\"redis\",\"ports\":[{\"containerPort\":27017,\"hostPort\":27017,\"name\":\"redis\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}},{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-02-24T20:23:56Z\",\"labels\":{\"name\":\"web\",\"region\":\"usa\"},\"name\":\"web-4c5bj\"},\"spec\":{\"containers\":[{\"image\":\"server\",\"name\":\"web\",\"ports\":[{\"containerPort\":3000,\"hostPort\":8080,\"name\":\"http-server\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}}],\"kind\":\"List\"}"

-- 9. Prepend the region to the name of each container.

-- prepending the region
e46 ∷ ByteString
e46 = pods & key "items" . values . reindexed (view (key "metadata" . key "labels" . key "region" . _String)) selfIndex <. (key "spec" . key "containers" . values . key "name" . _String) %@~ (\region name → region <> "-" <> name)

-- |
-- >>> e46
-- "{\"apiVersion\":\"v1\",\"items\":[{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-03-23T19:42:21Z\",\"labels\":{\"name\":\"redis\",\"region\":\"usa\"},\"name\":\"redis-h315w\"},\"spec\":{\"containers\":[{\"image\":\"redis\",\"name\":\"usa-redis\",\"ports\":[{\"containerPort\":27017,\"hostPort\":27017,\"name\":\"redis\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}},{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-02-24T20:23:56Z\",\"labels\":{\"name\":\"web\",\"region\":\"usa\"},\"name\":\"web-4c5bj\"},\"spec\":{\"containers\":[{\"image\":\"server\",\"name\":\"usa-web\",\"ports\":[{\"containerPort\":3000,\"name\":\"http-server\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}}],\"kind\":\"List\"}"
