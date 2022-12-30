-- {-# OPTIONS_GHC -ddump-splices #-}

module Optics where

import Control.Lens
  ( Each (each),
    Field2 (_2),
    Field3 (_3),
    beside,
    filtered,
    folded,
    over,
    partsOf,
    set,
    sumOf,
    traversed,
    worded,
    (%%~),
    (.~),
    (%~),
    (&),
    (*~),
    (^.),
    (^..),
    _Left,
    _head
  )
import Control.Lens.Extras (biplate)
import Data.Bits.Lens (bitAt)
import Data.Char (toUpper)
import Data.List (sort)
import Numeric.Lens (negated)

--------------------------------------------------------------------------------------------
--                                      2. Optics                                         --
--------------------------------------------------------------------------------------------

---------------------------------------
-- 2.4. Practical Optics at a Glance --
---------------------------------------

-- Update portions of immutable data structures.
e001 ∷ (Char, Char, Bool)
e001 = set _3 False ('a', 'b', 'c')

-- |
-- >>> e001
-- ('a','b',False)

-- We can perform a task over deeply nested subsets of data.
-- Let's sum all numbers in a `Left` within the right half of each tuple.
e002 ∷ Integer
e002 = sumOf (folded . _2 . _Left) [(True, Left 10), (False, Right "pepporoni"), (True, Left 20)]

-- |
-- >>> e002
-- 30

-- Truncate any stories longer than 10 characters, leaving shorter ones alone.
stories, e003 ∷ [String]
stories = ["This one time at band camp", "Nuff!", "This is a short story"]
e003 = over (traversed . filtered ((> 10) . length)) (\story → take 10 story ++ " ...") stories

-- |
-- >>> e003
-- ["This one t ...","Nuff!","This is a  ..."]

----------------------------------------
-- 2.5 Impractical Optics at a Glance --
----------------------------------------

-- Summarize a list of numbers, subtracting the 'Left's, adding the 'Right's!
e004 ∷ Integer
e004 = sumOf (folded . beside negated id) [Left 1, Right 10, Left 2, Right 20]

-- |
-- >>> e004
-- 27

e004' ∷ [Integer]
e004' = [Left 1, Right 10, Left 2, Right 20] ^.. (folded . beside negated id)

-- |
-- >>> e004'
-- [-1,10,-2,20]

e004'' ∷ [Integer]
e004'' = [Left 1, Right 10, Left 2, Right 20] ^.. (traversed . beside negated id)

-- |
-- >>> e004''
-- [-1,10,-2,20]

-- Capitalize each word in a sentence.
e005 ∷ String
e005 = "why is a raven like a writing desk" & worded . _head %~ toUpper

-- |
-- >>> e005
-- "Why Is A Raven Like A Writing Desk"

e005' ∷ String
e005' = "why is a raven like a writing desk" ^. worded

-- >>> e005'
-- "whyisaravenlikeawritingdesk"

e005'' ∷ [String]
e005'' = "why is a raven like a writing desk" ^.. worded

-- >>> e005''
-- ["why","is","a","raven","like","a","writing","desk"]

e005''' ∷ String
e005''' = "why is a raven like a writing desk" ^.. worded . _head

-- >>> e005'''
-- "wiarlawd"

-- Multiply every Integer by 100 no matter where they are in the structure.
e006 ∷ (Maybe Int, Either (String, [Int]) Int)
e006 = (Just 3, Left ("hello", [13, 15, 17])) & biplate *~ (100 ∷ Int)

-- |
-- >>> e006
-- (Just 300,Left ("hello",[1300,1500,1700]))

-- Reverse the ordering of all even numbers in a sequence.
-- We leave the odd numbers alone!
e007 ∷ [Integer]
e007 = [1, 2, 3, 4, 5, 6, 7, 8] & partsOf (traversed . filtered even) %~ reverse

-- |
-- >>> e007
-- [1,8,3,6,5,4,7,2]

-- Sort all the characters in all strings, across word boundaries!
e008 ∷ (String, String, String)
e008 = ("one", "two", "three") & partsOf (each . traversed) %~ sort

-- |
-- >>> e008
-- ("eee","hno","orttw")

-- >>> ("one", "two", "three") ^.. each . traversed
-- "onetwothree"

e008' ∷ [String]
e008' = ("one", "two", "three") ^.. each

-- |
-- >>> e008'
-- ["one","two","three"]

e008'' ∷ String
e008'' = ("one", "two", "three") ^. each

-- |
-- >>> e008''
-- "onetwothree"

e008''' ∷ [String]
e008''' = ["one", "two", "three"] & (traversed . each) .~ 'x'

-- |
-- >>> e008'''
-- ["xxx","xxx","xxxxx"]

e008'''' ∷ [String]
e008'''' = ["one", "two", "three"] & (each . traversed ) .~ 'x'

-- |
-- >>> e008''''
-- ["xxx","xxx","xxxxx"]

e008''''' :: (String, String, String)
e008''''' = ("one", "two", "three") & partsOf each %~ sort

-- |
-- >>> e008'''''
-- ("one","three","two")

-- Flip the 2nd bit of each number to a 0
e009 ∷ [Int]
e009 = [1, 2, 3, 4] & traversed . bitAt 1 %~ not

-- |
-- >>> e009
-- [3,0,1,6]

-- Prompt the user with each question in a tuple, then return the tuple with each prompt replaced with the user's input.
prompts ∷ (String, String, String)
prompts = ("What is your name?", "What is your quest?", "What is your favourite color?")

-- %%~ ⇒ "monadic" set
-- here we are in the IO monad
e010 ∷ IO (String, String, String)
e010 = prompts & each %%~ (\prompt → putStrLn prompt >> getLine)

-- What is your name?
-- > Sir Galahad
-- What is your quest?
-- > To seek the holy grail
-- What is your favourite color?
-- > Blue I think?
-- ("Sir Galahad","To seek the holy grail","Blue I think?")
