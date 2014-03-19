{-
 - This file shall be imported by Poxgram writers.
 -}
module Poxgram (
    pgMatch
  , pgHead
  , pgTail
  , pgAppend
  , pgEmpty
  , pgIfEmpty
  , pgTake
  , pgDrop
  , pgRepeat
  , pgAlternate
  ) where

import Comprehension
import PoxgramLanguage

-- Aliases for the data constructors.
pgMatch :: Comprehension -> Poxgram
pgMatch = Match

pgHead :: Poxgram -> Poxgram
pgHead = Head

pgTail :: Poxgram -> Poxgram
pgTail = Tail

pgAppend :: Poxgram -> Poxgram -> Poxgram
pgAppend = Sequence

pgEmpty :: Poxgram
pgEmpty = Empty

pgIfEmpty :: Poxgram -> Poxgram -> Poxgram -> Poxgram
pgIfEmpty = IfEmpty

-- Here we implement some list functions in the Poxgram datatype.
pgTake :: Int -> Poxgram -> Poxgram
pgTake 0 pg = pgEmpty
pgTake n pg = pgIfEmpty pg pg (pgAppend (pgHead pg) (pgTake (n-1) (pgTail pg)))

pgDrop :: Int -> Poxgram -> Poxgram
pgDrop 0 pg = pg
pgDrop n pg = pgIfEmpty pg pg (pgDrop (n-1) (pgTail pg))

pgRepeat :: Poxgram -> Poxgram
pgRepeat pg = pgAppend pg (pgRepeat pg)

pgAlternate :: Int -> Int -> Poxgram -> Poxgram -> Poxgram
pgAlternate n m p q = pgIfEmpty p q (pgAppend (pgTake n p) next)
  where next = pgAlternate m n q (pgDrop n p)
