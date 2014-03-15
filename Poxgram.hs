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
pgTake 0 pg = Empty
pgTake n pg = IfEmpty pg pg (Sequence (Head pg) (pgTake (n-1) (Tail pg)))

pgDrop :: Int -> Poxgram -> Poxgram
pgDrop 0 pg = pg
pgDrop n pg = IfEmpty pg pg (pgDrop (n-1) (Tail pg))

pgRepeat :: Poxgram -> Poxgram
pgRepeat pg = Sequence pg (pgRepeat pg)

pgAlternate :: Int -> Int -> Poxgram -> Poxgram -> Poxgram
pgAlternate n m p q = IfEmpty p q (Sequence (pgTake n p) next)
  where next = pgAlternate m n q (pgDrop n p)
