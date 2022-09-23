module Main where

import Control.Lens (asIndex, at, filtered, filteredBy, foldMapOf, has, ix, lengthOf, makeLensesFor, maximumByOf, only, reindexed, selfIndex, sumOf, toListOf, view, withIndex, (#), (%@~), (%~), (&), (*~), (<.), (?~), (^..), (^?), (^@..), _Nothing)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Lens (key, members, nth, values, _Array, _Double, _Integer, _JSON, _JSON', _Number, _Object, _String)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Text (toUpper)
import GHC.Generics (Generic)
import Text.RawString.QQ (r)

main ∷ IO ()
main = putStrLn "Applying Optics to JSON!"

------------------------
-- Introspecting JSON --
------------------------

-- `lens-aeson` provides prisms for all the major JSON subtypes ("json-like" types are represented as `t`):
-- _String ∷ Prism' t Text
-- _Bool   ∷ Prism' t Bool
-- _Null   ∷ Prism' t ()
-- _Object ∷ Prism' t (HashMap Text Value)
-- _Array  ∷ Prism' t (Vector Value)

-- Numeric prisms convert automatically the generic JSON number type:
-- _Number   ∷ Prism' t Scientific
-- _Double   ∷ Prism' t Double
-- _Integer  ∷ Prism' t Integer
-- _Integral ∷ Integral n ⇒ Prism' t n

-- |
-- The prisms will automatically parse values from strings into JSON.
-- >>> "42" ^? _Double
-- Just 42.0

-- |
-- >>> "42" ^? _Integer
-- Just 42

-- |
-- JSON strings must be quoted.
-- >>> "42" ^? _String
-- Nothing

-- |
-- >>> "\"42\"" ^? _String
-- Just "42"

-- |
-- >>> "\"Hello, World!\"" ^? _String
-- Just "Hello, World!"

-- |
-- Failure to parse a value fails the prism traversal.
-- >>> "{invalid JSON}" ^? _String
-- Nothing

-- |
-- If we mismatch the expected type it will fail even if the JSON is a well formed value of a different type.
-- >>> "\"Hello, World!\"" ^? _Double
-- Nothing

-- More structured JSON types using the `r` Quasi-Quoter to avoid escaping.
jsonObject ∷ String
jsonObject = [r| { "name": "Jack Sparrow", "rank": "Captain" } |]

jsonArray ∷ String
jsonArray = [r| [ "North", "East", "South", "West" ] |]

-- |
-- Maybe KeyMap
-- >>> jsonObject ^? _Object
-- Just (fromList [("name",String "Jack Sparrow"),("rank",String "Captain")])

-- |
-- Maybe Vector
-- >>> jsonArray ^? _Array
-- Just [String "North",String "East",String "South",String "West"]

-- By introducing some helpers (e.g. `key`, `nth`) we don't need to deal with KeyMaps or Vectors directly.

----------------------------------------
-- Diving Deeper into JSON Structures --
----------------------------------------

blackPearl ∷ String
blackPearl =
  [r| { "name": "Black Pearl",
        "crew": [ { "name": "Jack Sparrow",
                    "rank": "Captain" },
                  { "name": "Will Turner",
                    "rank": "First Mate" } ] } |]

-- |
-- We want to get the first crew member of the ship from its array of crew members.
-- This works, but it's long, hard to read.
-- >>> blackPearl ^? _Object . ix "crew" . _Array . ix 0 . _Object . ix "name" . _String
-- Just "Jack Sparrow"

-- |
-- Using the helpers `key` and `nth`.
-- >>> blackPearl ^? key "crew" . nth 0 . key "name" . _String
-- Just "Jack Sparrow"

-- definition of key
-- key ∷ Text → Traversal' t Value
-- key k = _Object . ix k

-- definition of nth
-- nth ∷ Int → Traversal' t Value
-- nth i = _Array . ix i

-- `key` and `nth` are traversals which use `ix`.
-- So they can't insert new values unless there's already a value at the given key.
-- To insert values into a JSON Object you can match on the Object and then use `at` like you would with a Map.
-- >>> blackPearl & _Object . at "name" ?~ "Purple Pearl"
-- "{\"crew\":[{\"name\":\"Jack Sparrow\",\"rank\":\"Captain\"},{\"name\":\"Will Turner\",\"rank\":\"First Mate\"}],\"name\":\"Purple Pearl\"}"

-------------------------------------------------
-- Traversing into Multiple JSON Substructures --
-------------------------------------------------

-- Sometimes we need to collect or modify values from many spots within a JSON structure.
-- E.g. we have a list of users and want to collect all of their email addresses.

-----------------------
-- Traversing Arrays --
-----------------------

-- traversing arrays with `values`
-- values ∷ AsValue t ⇒ IndexedTraversal' Int t Value
-- values = _Array . traversed

-- |
-- `values` allows us to traverse all the values in a Value if it's an Array.
-- >>> "[1, 2, 3]" ^.. values . _Integer
-- [1,2,3]

-- |
-- If any elements of the traversal don't match our assumptions they'll be SILENTLY EXCLUDED!
-- >>> "[1, null, 2, \"Hi mom!\", 3]" ^.. values . _Integer
-- [1,2,3]

-- |
-- If any elements of the traversal don't match our assumptions they'll be SILENTLY EXCLUDED!
-- >>> "[1, null, 2, \"Hi mom!\", 3]" ^.. values . _String
-- ["Hi mom!"]

-- |
-- `values` is an `IndexedTraversal' Int`.
-- So we have access to the array index even after we've descended into each element.
-- >>> "[\"a\", \"b\", \"c\"]" ^@.. values . _String
-- [(0,"a"),(1,"b"),(2,"c")]

-- |
-- >>> "[1, 2, 3]" ^@.. values . _Integer
-- [(0,1),(1,2),(2,3)]

-- |
-- It includes the correct array index even if we've filtered out values
-- >>> "[\"a\", 1, \"b\", null, \"c\", []]" ^@.. values . _String
-- [(0,"a"),(2,"b"),(4,"c")]

-- |
-- >>> "[\"a\", 1, \"b\", null, \"c\", [], 2]" ^@.. values . _Integer
-- [(1,1),(6,2)]

-- |
-- As long as we're chaining together Traversals and Prisms we can modify our focus.
-- >>> "[\"a\", \"b\", \"c\"]" & values . _String %~ toUpper
-- "[\"A\",\"B\",\"C\"]"

-- |
-- >>> "[1, 2, 3]" & values . _Integer %~ (+ 10)
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

-- |
-- >>> fleet ^.. values
-- [Object (fromList [("crew",Array [Object (fromList [("name",String "Jack Sparrow"),("rank",String "Captain")]),Object (fromList [("name",String "Will Turner"),("rank",String "First Mate")])]),("name",String "Black Pearl")]),Object (fromList [("crew",Array [Object (fromList [("name",String "Davy Jones"),("rank",String "Captain")]),Object (fromList [("name",String "Bootstrap Bill"),("rank",String "First Mate")])]),("name",String "Flying Dutchman")])]

-- |
-- all names of every ship in our fleet
-- >>> fleet ^.. values . key "name" . _String
-- ["Black Pearl","Flying Dutchman"]

-- |
-- names of all crew members in our entire fleet
-- >>> fleet ^.. values . key "crew" . values . key "name" . _String
-- ["Jack Sparrow","Will Turner","Davy Jones","Bootstrap Bill"]

-- |
-- include the ship each member is from alongside their name
-- >>> fleet ^@.. values . reindexed (view (key "name" . _String)) selfIndex <. (key "crew" . values . key "name" . _String)
-- [("Black Pearl","Jack Sparrow"),("Black Pearl","Will Turner"),("Flying Dutchman","Davy Jones"),("Flying Dutchman","Bootstrap Bill")]

------------------------
-- Traversing Objects --
------------------------

-- traversing objects with `members`
-- members ∷ AsValue t ⇒ IndexedTraversal' Text t Value

cargo ∷ String
cargo =
  [r| { "emeralds": 327,
              "rubies": 480,
              "sapphires": 621,
              "opals": 92,
              "dubloons": 34 } |]

-- |
-- count of all the items (ordered by keys)
-- >>> cargo ^.. members . _Integer
-- [34,327,92,480,621]

-- |
-- sum of all items
-- >>> sumOf (members . _Integer) cargo
-- 1554

-- |
-- getting the item name alongside the quantity
-- `_Integer` is an index-preserving optic so we don't need to muck about with `(<.)`.
-- >>> cargo ^@.. members . _Integer
-- [("dubloons",34),("emeralds",327),("opals",92),("rubies",480),("sapphires",621)]

-- |
-- item with the most number of items ;-)
-- Unfortunately there's no `imaximumOf` so we'll need to explicitly include the index into the fold using `withIndex`.
-- >>> maximumByOf (members . _Integer . withIndex) (comparing snd) cargo
-- Just ("sapphires",621)

----------------------------
-- Filtering JSON Queries --
----------------------------

-- The trick with this sort of query is that we may be filtering on a different aspect than we return from the query.
-- In this case we only want to focus the name of the crew member, but we're filtering on the crew member's rank.
-- This means we need to run our filter in the middle of our path before we've lost access to the information we need.
-- ⇒ use `filtered` and `filteredBy`

-- |
-- all captains
-- `OverloadedStrings` allows our "Captain" string to be changed into a JSON string Value which is compared to the rank's Value.
-- >>> fleet ^.. values . key "crew" . values . filteredBy (key "rank"           . only "Captain") . key "name" . _String
-- ["Jack Sparrow","Davy Jones"]

-- |
-- all captains
--                                                     explicit conversion without using OverloadedStrings
--                                                                          |
-- >>> fleet ^.. values . key "crew" . values . filteredBy (key "rank" . _String . only "Captain") . key "name" . _String
-- ["Jack Sparrow","Davy Jones"]

-- |
-- all captains
-- using `filtered` instead of `filteredBy`
-- >>> fleet ^.. values . key "crew" . values . filtered (has (key "rank" . only "Captain")) . key "name" . _String
-- ["Jack Sparrow","Davy Jones"]

-------------------------------------------------------
-- Serializing & Deserializing within an Optics Path --
-------------------------------------------------------

-- In Haskell we would typically prefer to work with strongly typed records rather than nebulously typed Value objects.
-- Let's see how we can reflect values from their Value representations into their structured Haskell forms and back within a Traversal.

-- record type with ToJSON and FromJSON instances
data Creature = Creature
  { creatureName ∷ String,
    creatureSize ∷ Double
  }
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

--                field name       lens name
--                     |               |
makeLensesFor [("creatureName", "creatureNameL"), ("creatureSize", "creatureSizeL")] ''Creature

-- The `_JSON'` prism can convert between any JSON-like `t` and any type `a` which can be serialized to or from a JSON value:
-- _JSON' ∷ (AsJSON t, FromJSON a, ToJSON a) ⇒ Prism' t a

-- |
-- Since the result type of _JSON' is polymorphic over anything that implements ToJSON and FromJSON we'll usually need type annotations.
-- >>> creatureSightings ^.. members . values . _JSON' ∷ [Creature]
-- [Creature {creatureName = "Giant Squid", creatureSize = 45.2},Creature {creatureName = "Kraken", creatureSize = 124.4},Creature {creatureName = "Ogopogo", creatureSize = 34.6}]

-- |
-- using TypeApplications; @_ says that we don't really care what we're converting from!
-- >>> creatureSightings & members . values . _JSON @_ @Creature . creatureSizeL *~ 1.2
-- "{\"Arctic\":[{\"creatureName\":\"Giant Squid\",\"creatureSize\":54.24}],\"Pacific\":[{\"creatureName\":\"Kraken\",\"creatureSize\":149.28},{\"creatureName\":\"Ogopogo\",\"creatureSize\":41.52}]}"

-- |
-- From the use of creatureSizeL GHC can actually infer the type we want to convert to.
-- Error messages could be more useful with type annotations, though.
-- >>> creatureSightings & members . values . _JSON @_ @_ . creatureSizeL *~ 1.2
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
                    "region": "usa"
                }
            },
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
                            }
                        ],
                        "resources": {
                            "requests": {
                                "cpu": "100m"
                            }
                        }
                    }
                ]
            }
        },
        {
            "kind": "Pod",
            "apiVersion": "v1",
            "metadata": {
                "name": "web-4c5bj",
                "creationTimestamp": "2019-02-24T20:23:56Z",
                "labels": {
                    "name": "web",
                    "region": "usa"
                }
            },
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
                            }
                        ],
                        "resources": {
                            "requests": {
                                "cpu": "100m"
                            }
                        }
                    }
                ]
            }
        }
    ]
}
|]

-- 1. Your first task should be mostly straightforward: get the api version which was used to make the call.

-- |
-- >>> pods ^? key "apiVersion" . _String
-- Just "v1"

-- 2. Next, count the number of all containers across all pods. You can assume that every element of "items" is a pod.

-- |
-- >>> lengthOf (key "items" . values . key "spec" . key "containers" . values) pods
-- 2

-- 3. Return the "name" (as Text) of all containers which have the same value for their "image" and "name" fields.

-- |
-- >>> toListOf (key "items" . values . key "spec" . key "containers" . values . filtered (\v → v ^? key "name" == v ^? key "image") . key "name" . _String) pods
-- ["redis"]

-- 4. Collect a list of all "containerPort"s alongside their Pod's "metadata > name".

-- |
-- >>> toListOf ((key "items" . values . reindexed (view (key "metadata" . key "name" . _String)) selfIndex <. (key "spec" . key "containers" . values . key "ports" . values . key "containerPort" . _Integer)) . withIndex) pods
-- [("redis-h315w",27017),("web-4c5bj",3000)]

-- 5. Uppercase the label values inside each pod's metadata.

-- |
-- >>> pods & key "items" . values . key "metadata" . key "labels" . members . _String %~ toUpper
-- "{\"apiVersion\":\"v1\",\"items\":[{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-03-23T19:42:21Z\",\"labels\":{\"name\":\"REDIS\",\"region\":\"USA\"},\"name\":\"redis-h315w\"},\"spec\":{\"containers\":[{\"image\":\"redis\",\"name\":\"redis\",\"ports\":[{\"containerPort\":27017,\"hostPort\":27017,\"name\":\"redis\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}},{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-02-24T20:23:56Z\",\"labels\":{\"name\":\"WEB\",\"region\":\"USA\"},\"name\":\"web-4c5bj\"},\"spec\":{\"containers\":[{\"image\":\"server\",\"name\":\"web\",\"ports\":[{\"containerPort\":3000,\"name\":\"http-server\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}}],\"kind\":\"List\"}"

-- 6. Set a resource request of memory: "256M" for every container.

-- |
-- >>> pods & key "items" . values . key "spec" . key "containers" . values . key "resources" . key "requests" . _Object . at "memory" ?~ "256M"
-- "{\"apiVersion\":\"v1\",\"items\":[{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-03-23T19:42:21Z\",\"labels\":{\"name\":\"redis\",\"region\":\"usa\"},\"name\":\"redis-h315w\"},\"spec\":{\"containers\":[{\"image\":\"redis\",\"name\":\"redis\",\"ports\":[{\"containerPort\":27017,\"hostPort\":27017,\"name\":\"redis\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\",\"memory\":\"256M\"}}}]}},{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-02-24T20:23:56Z\",\"labels\":{\"name\":\"web\",\"region\":\"usa\"},\"name\":\"web-4c5bj\"},\"spec\":{\"containers\":[{\"image\":\"server\",\"name\":\"web\",\"ports\":[{\"containerPort\":3000,\"name\":\"http-server\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\",\"memory\":\"256M\"}}}]}}],\"kind\":\"List\"}"

-- BONUS Questions

-- These problems require a bit more thought, and will probably require some unorthodox uses of indexes and filtering. Good luck!
-- Don't get too frustrated, they're deliberately very hard!

-- 7. Get a Set of all metadata label keys used in the response.

-- |
-- >>> foldMapOf (key "items" . values . key "metadata" . key "labels" . members . asIndex) S.singleton pods
-- fromList ["name","region"]

-- 8. Set the hostPort to 8080 on any "port" descriptions where it is unset.

-- |
-- >>> pods & key "items" . values . key "spec" . key "containers" . values . key "ports" . values . _Object . at "hostPort" . filteredBy _Nothing ?~ _Number # 8080
-- "{\"apiVersion\":\"v1\",\"items\":[{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-03-23T19:42:21Z\",\"labels\":{\"name\":\"redis\",\"region\":\"usa\"},\"name\":\"redis-h315w\"},\"spec\":{\"containers\":[{\"image\":\"redis\",\"name\":\"redis\",\"ports\":[{\"containerPort\":27017,\"hostPort\":27017,\"name\":\"redis\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}},{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-02-24T20:23:56Z\",\"labels\":{\"name\":\"web\",\"region\":\"usa\"},\"name\":\"web-4c5bj\"},\"spec\":{\"containers\":[{\"image\":\"server\",\"name\":\"web\",\"ports\":[{\"containerPort\":3000,\"hostPort\":8080,\"name\":\"http-server\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}}],\"kind\":\"List\"}"

-- 9. Prepend the region to the name of each container.

-- |
-- >>> pods & key "items" . values . reindexed (view (key "metadata" . key "labels" . key "region" . _String)) selfIndex <. ( key "spec" . key "containers" . values . key "name" . _String) %@~ (\region name → region <> "-" <> name)
-- "{\"apiVersion\":\"v1\",\"items\":[{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-03-23T19:42:21Z\",\"labels\":{\"name\":\"redis\",\"region\":\"usa\"},\"name\":\"redis-h315w\"},\"spec\":{\"containers\":[{\"image\":\"redis\",\"name\":\"usa-redis\",\"ports\":[{\"containerPort\":27017,\"hostPort\":27017,\"name\":\"redis\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}},{\"apiVersion\":\"v1\",\"kind\":\"Pod\",\"metadata\":{\"creationTimestamp\":\"2019-02-24T20:23:56Z\",\"labels\":{\"name\":\"web\",\"region\":\"usa\"},\"name\":\"web-4c5bj\"},\"spec\":{\"containers\":[{\"image\":\"server\",\"name\":\"usa-web\",\"ports\":[{\"containerPort\":3000,\"name\":\"http-server\",\"protocol\":\"TCP\"}],\"resources\":{\"requests\":{\"cpu\":\"100m\"}}}]}}],\"kind\":\"List\"}"
