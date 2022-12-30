-- {-# OPTIONS_GHC -ddump-splices #-}

module IndexedOptics where

import Control.Lens
  ( Each (each),
    Field1 (_1),
    Field2 (_2),
    Indexable (indexed),
    IndexedFold,
    IndexedTraversal,
    Prefixed (prefixed),
    both,
    cloneIndexPreservingLens,
    folded,
    has,
    icompose,
    ifolding,
    index,
    indexing,
    indices,
    itoListOf,
    itraverseOf_,
    itraversed,
    ix,
    lined,
    reindexed,
    selfIndex,
    sumOf,
    toListOf,
    traversed,
    (&),
    (.~),
    (<.),
    (<.>),
    (^..),
    (^?),
    (^@..),
    (%@~)
  )
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree (Node))

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

-- |
-- the same
-- >>> ["Summer", "Fall", "Winter", "Spring"] ^@.. folded
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
