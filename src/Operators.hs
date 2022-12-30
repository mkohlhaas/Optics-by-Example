-- {-# OPTIONS_GHC -ddump-splices #-}

module Operators where

import Control.Lens
  ( Field1 (_1),
    Field2 (_2),
    makeLenses,
    over,
    set,
    view,
    (%~),
    (&),
    (&&~),
    (*~),
    (+~),
    (-~),
    (.~),
    (//~),
    (<+~),
    (<<+~),
    (<>~),
    (<<>~),
    (^.),
    (^~),
    (||~),
  )
import Data.Char (toUpper)

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
