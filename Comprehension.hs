module Comprehension (
    Comprehension
  , artist
  , album
  , title
  , year
  , (<|>)
  , (<&>)
  ) where

-- The desired interface:
--
--   Users work only with one type. They get AND and OR combinators,
--   as well as pure constructors for each field in a mk.
--
-- However, we will have to expose internals in order for interpreter
-- writing to be possible!
--
-- Ok, we'll define Conjunct and Disjunct in one file, intended for use by
-- the interpreter writers. Then, we'll define Comprehension, intended for
-- use by the program writers.

import Types
import Disjunct
import Conjunct

type Comprehension = Disjunct

artist :: Artist -> Disjunct
artist = disjunctSingle . mkArtist

album :: Album -> Disjunct
album = disjunctSingle . mkAlbum

title :: Title -> Disjunct
title = disjunctSingle . mkTitle

year :: Year -> Disjunct
year = disjunctSingle . mkYear

(<|>) = disjunctOr
(<&>) = disjunctAnd

--applyComprehension :: (Constraint a -> Constraint a -> Constraint a)
--                   -> Comprehension -> Comprehension -> Comprehension
--
-- INTERESTING limitation found here! Evidently, since f is used on the
-- Constraint Artist type, its output must be fixed! That is, the third
-- Constraint a in the type above must be fixed at Constraint Artist! That's
-- not what I want, though... I want to be able to lift an ad-hoc polymorphic
-- function into here. Not possible!
-- Perhaps shoot this one out to Haskell Café?
--   
--applyComprehension f x y = C {
--    _artist = (_artist x) `f` (_artist y)
--  , _title = (_title x) `f` (_title y)
--  , _year = (_year x) `f` (_year y)
--  }
