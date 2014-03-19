{-
 - Here we define a typeclass and functions related to interpreting (playing
 - back) a Poxgram.
 -}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module PoxgramInterpreter (
    PoxgramInterpreter
  , atomizeComprehension
  , playAtoms
  , runPoxgram
  ) where

import Control.Applicative
import Control.Monad

import Comprehension
import PoxgramLanguage

class Monad m => PoxgramInterpreter a m b | a -> m b where
  atomizeComprehension :: a -> Comprehension -> m [b]
  -- playSong must block until one song song is over or interrupted, then
  -- continue to the next one.
  playAtoms :: a -> m [b] -> IO ()

runPoxgram :: (PoxgramInterpreter a m b) => a -> Poxgram -> IO ()
runPoxgram x = (playAtoms x) . (atomizePoxgram x)

-- | Playback a poxgram.
atomizePoxgram :: (PoxgramInterpreter a m b) => a -> Poxgram -> m [b]
atomizePoxgram x (Match c) = atomizeComprehension x c
atomizePoxgram x Empty = return []
atomizePoxgram x (Head pg) = do
  theList <- atomizePoxgram x pg
  return  $ safeHead theList
atomizePoxgram x (Tail pg) = do
  theList <- atomizePoxgram x pg
  return $ safeTail theList
atomizePoxgram x (Sequence p q) = do
  first <- atomizePoxgram x p
  second <- atomizePoxgram x q
  return $ first ++ second
atomizePoxgram x (IfEmpty p q r) = do
  evaluatedP <- atomizePoxgram x p
  case evaluatedP of
    [] -> atomizePoxgram x q
    _ -> atomizePoxgram x r

safeHead [] = []
safeHead (x:xs) = [x]

safeTail [] = []
safeTail (x:xs) = xs
