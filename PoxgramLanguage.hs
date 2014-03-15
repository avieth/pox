{-
 - Here we define the Poxgram language itself.
 -}

{-# LANGUAGE GADTs #-}

module PoxgramLanguage (
    Poxgram(..)
  ) where

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
  --   NB this is the only constructor which allows for a Poxgram to grow.
  Sequence :: Poxgram -> Poxgram -> Poxgram
  -- | Choose alternative Poxgrams depending upon whether one Poxgram is
  --   empty. If the first argument is empty, the second should be taken,
  --   otherwise the third should be taken.
  IfEmpty :: Poxgram -> Poxgram -> Poxgram -> Poxgram
