module Comprehension (
    Comprehension
  , artist
  , album
  , title
  , date
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

date :: Date -> Disjunct
date = disjunctSingle . mkDate

(<|>) = disjunctOr
(<&>) = disjunctAnd
