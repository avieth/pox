{-
 - Here we define a typeclass and functions related to interpreting (playing
 - back) a Poxgram.
 -}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module PoxgramInterpreter (
    PoxgramInterpreter
  , atomizeComprehension
  , playAtoms
 -- , runPoxgram
  ) where

import Control.Applicative
import Control.Monad

import Comprehension
import PoxgramLanguage

class PoxgramInterpreter a b | a -> b where
  atomizeComprehension :: a -> Comprehension -> IO [b]
  -- playSong must block until one song song is over or interrupted, then
  -- continue to the next one.
  playAtoms :: a -> IO [b] -> IO ()

-- | Playback a poxgram.
atomizePoxgram :: (PoxgramInterpreter a b) => a -> Poxgram -> IO [b]
atomizePoxgram x (Match c) = atomizeComprehension x c
atomizePoxgram x Empty = return []
-- We have to take the head at two levels: once in the outer list and once in
-- the inner (inside the functor b) list. If the outer list is empty, we can
-- just give the empty list. Otherwise, we must find the first nonempty list
-- inside the functor, and give back its head. Can this be done with just a
-- plain functor? I don't think so... We'll need an applicative!
--atomizePoxgram x (Head pg) = extractHead $ atomizePoxgram x pg
--atomizePoxgram x (Tail pg) = extractTail $ atomizePoxgram x pg
atomizePoxgram x (Head pg) = do
  theList <- atomizePoxgram x pg
  return  $ safeHead theList
atomizePoxgram x (Tail pg) = do
  theList <- atomizePoxgram x pg
  return $ safeTail theList
-- This one will fail for repeat, no?
atomizePoxgram x (Sequence p q) = do
  first <- atomizePoxgram x p
  second <- atomizePoxgram x q
  return $ first ++ second
-- Not correct! The Poxgram [m []] is also empty!
-- Evidently we need the container to be at least an Applicative, and we need
-- to be able to exit it in the interpreter!
atomizePoxgram x (IfEmpty p q r) = do
  evaluatedP <- atomizePoxgram x p
  case evaluatedP of
    [] -> atomizePoxgram x q
    _ -> atomizePoxgram x r

safeHead [] = []
safeHead (x:xs) = [x]

safeTail [] = []
safeTail (x:xs) = xs
