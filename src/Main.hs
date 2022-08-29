module Main where

import Control.Lens (at, filteredBy, ix, makeLensesFor, maximumByOf, only, reindexed, selfIndex, sumOf, view, withIndex, (%~), (&), (*~), (<.), (?~), (^..), (^?), (^@..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Lens (key, members, nth, values, _Array, _Double, _Integer, _JSON, _JSON', _Object, _String)
import Data.Text (toUpper)
import GHC.Generics (Generic)
import Text.RawString.QQ (r)

main ∷ IO ()
main = putStrLn "Applying Optics to JSON!"

------------------------
-- Introspecting JSON --
------------------------

-- |
-- The prisms will automatically parse values from strings into JSON
-- >>> "42" ^? _Double
-- Just 42.0

-- |
-- >>> "42" ^? _Integer
-- Just 42

-- |
-- >>> "42" ^? _String
-- Nothing

-- |
-- Failure to parse a value fails the prism traversal
-- >>> "{invalid JSON}" ^? _String
-- Nothing

-- |
-- >>> "\"Hello, World!\"" ^? _String
-- Just "Hello, World!"

-- |
-- If we mismatch the expected type it will fail even if the JSON is a well formed value of a different type.
-- >>> "\"Hello, World!\"" ^? _Double
-- Nothing
jsonObject ∷ String
jsonObject = [r| { "name": "Jack Sparrow", "rank": "Captain" } |]

jsonArray ∷ String
jsonArray = [r| [ "North", "East", "South", "West" ] |]

-- |
-- >>> jsonObject ^? _Object
-- Just (fromList [("name",String "Jack Sparrow"),("rank",String "Captain")])

-- |
-- >>> jsonArray ^? _Array
-- Just [String "North",String "East",String "South",String "West"]

----------------------------------------
-- Diving Deeper into JSON Structures --
----------------------------------------

blackPearl ∷ String
blackPearl =
  [r| { "name": "Black Pearl",
                   "crew": [ { "name": "Jack Sparrow", "rank": "Captain" },
                             { "name": "Will Turner", "rank": "First Mate" } ] } |]

-- |
-- We want to get the first crew member of the ship from its array of crew members.
-- This works, but it's long, hard to read.
-- >>> blackPearl ^? _Object . ix "crew" . _Array . ix 0 . _Object . ix "name" . _String
-- Just "Jack Sparrow"

-- |
-- Using `key` and `nth`.
-- >>> blackPearl ^? key "crew" . nth 0 . key "name" . _String
-- Just "Jack Sparrow"

-- >>> myJSON & _Object . at "newKey" ?~ newValue

-- |
-- Note that `key` and `nth` are traversals which use `ix`; so they can't insert new values unless there's already a value at the given key.
-- To insert values into a JSON Object you can match on the Object then use `at` like you would with a Map.
-- >>> blackPearl & _Object . at "name" ?~ "Purple Pearl"
-- "{\"crew\":[{\"name\":\"Jack Sparrow\",\"rank\":\"Captain\"},{\"name\":\"Will Turner\",\"rank\":\"First Mate\"}],\"name\":\"Purple Pearl\"}"

-------------------------------------------------
-- Traversing into Multiple JSON Substructures --
-------------------------------------------------

-----------------------
-- Traversing Arrays --
-----------------------

-- |
-- `values` allows us to traverse all the values in a Value if it's an Array
-- >>> "[1, 2, 3]" ^.. values . _Integer
-- [1,2,3]

-- |
-- if any elements of the traversal don't match our assumptions they'll be silently excluded
-- >>> "[1, null, 2, \"Hi mom!\", 3]" ^.. values . _Integer
-- [1,2,3]

-- |
-- Using the indexed `itoListOf` operator `^@..`.
-- >>> "[\"a\", \"b\", \"c\"]" ^@.. values . _String
-- [(0,"a"),(1,"b"),(2,"c")]

-- |
-- It includes the correct array index even if we've filtered out values
-- >>> "[\"a\", 1, \"b\", null, \"c\", []]" ^@.. values . _String
-- [(0,"a"),(2,"b"),(4,"c")]

-- |
-- As long as we're chaining together Traversals and Prisms we can modify our focus.
-- >>> "[\"a\", \"b\", \"c\"]" & values . _String %~ toUpper
-- "[\"A\",\"B\",\"C\"]"

-- a more complex example
fleet ∷ String
fleet =
  [r| [ { "name": "Black Pearl",
              "crew": [ { "name": "Jack Sparrow", "rank": "Captain" },
                        { "name": "Will Turner", "rank": "First Mate" } ] },
        { "name": "Flying Dutchman",
              "crew": [ { "name": "Davy Jones", "rank": "Captain" },
                        { "name": "Bootstrap Bill", "rank": "First Mate" } ] } ] |]

-- |
-- >>> fleet ^.. values
-- [Object (fromList [("crew",Array [Object (fromList [("name",String "Jack Sparrow"),("rank",String "Captain")]),Object (fromList [("name",String "Will Turner"),("rank",String "First Mate")])]),("name",String "Black Pearl")]),Object (fromList [("crew",Array [Object (fromList [("name",String "Davy Jones"),("rank",String "Captain")]),Object (fromList [("name",String "Bootstrap Bill"),("rank",String "First Mate")])]),("name",String "Flying Dutchman")])]

-- |
-- >>> fleet ^.. values . key "name" . _String
-- ["Black Pearl","Flying Dutchman"]

-- |
-- >>> fleet ^.. values . key "crew" . values . key "name" . _String
-- ["Jack Sparrow","Will Turner","Davy Jones","Bootstrap Bill"]

-- |
-- >>> fleet ^@.. values . reindexed (view (key "name" . _String)) selfIndex <. (key "crew" . values . key "name" . _String)
-- [("Black Pearl","Jack Sparrow"),("Black Pearl","Will Turner"),("Flying Dutchman","Davy Jones"),("Flying Dutchman","Bootstrap Bill")]

------------------------
-- Traversing Objects --
------------------------

cargo ∷ String
cargo = [r| { "emeralds": 327, "rubies": 480, "sapphires": 621, "opals": 92, "dubloons": 34 } |]

-- |
-- `members` focuses each Value in an Object.
-- >>> cargo ^.. members . _Integer
-- [34,327,92,480,621]

-- |
-- >>> sumOf (members . _Integer) cargo
-- 1554

-- |
-- >>> cargo ^@.. members . _Integer
-- [("dubloons",34),("emeralds",327),("opals",92),("rubies",480),("sapphires",621)]

-- |
-- Unfortunately there's no `imaximumOf` so we'll need to explicitly include the index into the fold using `withIndex`.
-- >>> import Data.Ord (comparing)
-- >>> maximumByOf (members . _Integer . withIndex) (comparing snd) cargo
-- Just ("sapphires",621)

----------------------------
-- Filtering JSON Queries --
----------------------------

-- |
-- Get all captains.
-- >>> fleet ^.. values . key "crew" . values . filteredBy (key "rank" . only "Captain") . key "name" . _String
-- ["Jack Sparrow","Davy Jones"]

-------------------------------------------------------
-- Serializing & Deserializing within an Optics Path --
-------------------------------------------------------

data Creature = Creature
  { creatureName ∷ String,
    creatureSize ∷ Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

creatureSightings ∷ String
creatureSightings =
  [r| { "Arctic": [ { "creatureName": "Giant Squid", "creatureSize": 45.2 } ],
       "Pacific": [ { "creatureName": "Kraken", "creatureSize": 124.4 },
                    { "creatureName": "Ogopogo", "creatureSize": 34.6 } ] } |]

--               field name      lens name
makeLensesFor [("creatureName", "creatureNameL"), ("creatureSize", "creatureSizeL")] ''Creature

-- The `_JSON'` prism is a prism which can convert between any JSON-like `t` and any type `a` which can be serialized to or from a JSON value:
-- _JSON' ∷ (AsJSON t, FromJSON a, ToJSON a) ⇒ Prism' t a

-- |
-- Since the result type of _JSON' is polymorphic over anything that implements ToJSON and FromJSON we'll usually need type annotations.
-- >>> creatureSightings ^.. members . values . _JSON' ∷ [Creature]
-- [Creature {creatureName = "Giant Squid", creatureSize = 45.2},Creature {creatureName = "Kraken", creatureSize = 124.4},Creature {creatureName = "Ogopogo", creatureSize = 34.6}]

-- |
-- Using TypeApplications.
-- @_ says that we don’t really care what we’re converting FROM!
-- >>> creatureSightings & members . values . _JSON @_ @Creature . creatureSizeL *~ 1.2
-- "{\"Arctic\":[{\"creatureName\":\"Giant Squid\",\"creatureSize\":54.24}],\"Pacific\":[{\"creatureName\":\"Kraken\",\"creatureSize\":149.28},{\"creatureName\":\"Ogopogo\",\"creatureSize\":41.52}]}"
