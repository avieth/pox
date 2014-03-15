{-
 - Here we define a typeclass and functions related to interpreting (playing
 - back) a Poxgram.
 -}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module PoxgramInterpreter (
    PoxgramInterpreter
  , atomizeComprehension
  , playSong
  , runPoxgram
  ) where

import Control.Applicative
import Control.Monad

import Comprehension
import PoxgramLanguage

-- | An interpreter for a Poxgram is any instance of this class.
--   The type a represents individual tracks for playblack, and the type m
--   is the monad (almost certainly with IO capabilities) in which those
--   tracks will be retrieved and played back.
class (Functor m, Monad m) => PoxgramInterpreter m a | m -> a where
  atomizeComprehension :: Comprehension -> m [a]
  -- playSong must block until the song is over or interrupted
  playSong :: a -> m ()

-- | Given a PoxgramInterpreter we can play back an atomized Poxgram.
play :: (PoxgramInterpreter m a) => [m [a]] -> m ()
play [] = return ()
play (x:xs) = do
  songs <- x
  forM_ songs playSong
  play xs

-- | Given a PoxgramInterpreter we can atomize a Poxgram.
atomize :: (PoxgramInterpreter m a) => Poxgram -> [m [a]]
atomize = atomizePoxgram atomizeComprehension

-- | To run a Poxgram, we just atomize it and then play!
runPoxgram :: (PoxgramInterpreter m a) => Poxgram -> m ()
runPoxgram = play . atomize

-- | Transform a Poxgram into a list of things which can be played back.
--   Each Match constructor will invoke a function which produces a list of
--   some playable thing within some monad; this must be supplied by the
--   interpreter writer. We wrap those monadic values in a list because we
--   don't want to evaluate Sequence constructors through the bind, as that
--   would make an infinite Poxgram not playable. Since we use a list, we don't
--   need to evaluate both arguments to Sequence completely, and so infinite
--   Poxgrams are admissible.
atomizePoxgram :: (Functor m, Monad m)
  => (Comprehension -> m [a])
  -> Poxgram
  -> [m [a]]
atomizePoxgram atm (Match c) = [atm c]
atomizePoxgram atm Empty = [return []]
atomizePoxgram atm (Head pg) = [safeHead <$> (head $ atomizePoxgram atm pg)]
atomizePoxgram atm (Tail pg) = safeTail' $ atomizePoxgram atm pg
atomizePoxgram atm (Sequence p q) = (atomizePoxgram atm p) ++ (atomizePoxgram atm q)
atomizePoxgram atm (IfEmpty p q r) = case atomizePoxgram atm p of
  [] -> atomizePoxgram atm q
  _ -> atomizePoxgram atm r

safeHead [] = []
safeHead x = [head x]

safeTail [] = []
safeTail (x:xs) = xs

-- safeTail lifted into the head of a list of functors.
safeTail' [] = []
safeTail' (x:xs) = (safeTail <$> x) : xs
