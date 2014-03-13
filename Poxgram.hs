{-# LANGUAGE GADTs #-}

module Poxgram (
    Poxgram(..)
  ) where

{-
 - Overview: a Poxgram describes some tracks to play. It does not play any
 - tracks, though, for this must be done by an interpreter.
 -
 - An interpreter for a Poxgram will be very simple: convert the Poxgram
 - into a normal Haskell list of some datatype t, then play all of those
 - datatypes in order. That is to say, an interpreter must implement:
 -
 -   atomize :: Poxgram -> [t]
 -   playAtom :: t -> IO ()
 -
 - so that evaluation is just
 -
 -   (forM_ playAtom) . atomize
 -
 - But in fact, atomize need only be 
 -
 -   atomize :: Comprehension -> [t]
 -
 - because
 -
 -   atomizePoxgram (Match c) = atomizeComprehension c
 -   atomizePoxgram (Empty) = []
 -   atomizePoxgram (Head p) = [head $ atomize p]
 -   atomizePoxgram (Tail p) = tail $ atomize p
 -   atomizePoxgram (Sequence p q) = (atomize p) ++ (atomize q)
 -   atomizePoxgram (IfEmpty p q r) = case atomizePoxgram p of
 -     [] -> atomizePoxgram q
 -     _  -> atomizePoxgram r
 -
 - Cool huh? This seems like a solid design: use the typical Haskell list
 - functions, exposing only the essential ones in the Poxgram datatype.
 -}

import Comprehension

-- | A Poxgram is a representation of some simple list functions in data.
data Poxgram where
  Empty :: Poxgram
  -- | Most basic nonempty Poxgram: just carry a comprehension.
  Match :: Comprehension -> Poxgram
  -- | Extract the first atom in a Poxgram.
  Head :: Poxgram -> Poxgram
  -- | Remove the first atom from a Poxgram.
  Tail :: Poxgram -> Poxgram
  -- | Play one Poxgram followed by another; corresponds to list append.
  Sequence :: Poxgram -> Poxgram -> Poxgram
  -- | Choose alternative Poxgrams depending upon whether one Poxgram is
  --   empty.
  IfEmpty :: Poxgram -> Poxgram -> Poxgram -> Poxgram

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
