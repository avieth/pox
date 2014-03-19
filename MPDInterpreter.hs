-- TBD these are necessary?
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module MPDInterpreter (
    MPDInterpreter
  , mpdInterpreter
  ) where

import Control.Monad
import Control.Applicative
import Data.String (fromString)

import Network.MPD

import Comprehension hiding ((<&>))
import Conjunct
import Disjunct

import PoxgramInterpreter

-- | The MPDInterpreter type.
-- TODO parameterize on some MPD connection parameters.
data MPDInterpreter = MkMPDInterpreter

mpdInterpreter :: MPDInterpreter
mpdInterpreter = MkMPDInterpreter

instance PoxgramInterpreter MPDInterpreter MPD Song where
  atomizeComprehension = \x -> mpdAtomizeComprehension
  playAtoms = \x -> mpdPlaySongs

-- | The goal implementation: play a song and block until its finished.
--   Aha but MPD does not play songs, it plays a playlist.
--   This will have to clear the playlist, put the song in, and hit play.
playSong :: Song -> MPD ()
playSong s = do
  clear
  add_ $ sgFilePath s
  -- play takes a Maybe Int. I believe it indicates the position in the
  -- song to start at. Not sure though, as it's undocumented.
  -- We give Nothing; hopefully that means start from the beginning
  play Nothing

playSongAndBlock :: Song -> MPD ()
playSongAndBlock s = do
  playSong s
  -- This is dubious. We may have a race!
  -- We want to block until the song is over; how can we do that with MPD's
  -- api? I figure we just block until the player subsystem has a state change.
  -- However, playSong s will induce that, assuming MPD gets the message
  -- before this next line is executed. So we idle again after that.
  idle [PlayerS]
  idle [PlayerS]
  return ()

playSongs' :: [Song] -> MPD ()
playSongs' songs = forM_ songs playSongAndBlock

mpdPlaySongs :: MPD [Song] -> IO ()
mpdPlaySongs xs = (const ()) <$> (withMPD action)
  where action = do {
    songs <- xs;
    playSongs' songs;
  }

-- | Produce a list of Songs from a Comprehension, inside some MonadMPD.
mpdAtomizeComprehension :: (MonadMPD m) => Comprehension -> m [Song]
mpdAtomizeComprehension = (liftM concat) . ((flip forM) find) . queriesFromComprehension

queriesFromComprehension :: Comprehension -> [Query]
queriesFromComprehension = queriesFromConjuncts . getConjuncts

queriesFromConjuncts :: [Conjunct] -> [Query]
queriesFromConjuncts = map queryFromConjunct

queryFromConjunct :: Conjunct -> Query
queryFromConjunct c = 
  artistFromConjunct c
  <&> albumFromConjunct c
  <&> titleFromConjunct c
  <&> yearFromConjunct c

artistFromConjunct c = case getArtist c of
  Just a -> Artist =? fromString a
  Nothing -> anything

albumFromConjunct c = case getAlbum c of
  Just a -> Album =? fromString a
  Nothing -> anything

titleFromConjunct c = case getTitle c of
  Just t -> Title =? fromString t
  Nothing -> anything

yearFromConjunct c = case getYear c of
  -- Oops, conjunct should use date, not year.
  -- TBD best way to convert a number to a string then a bytestring?
  Just y -> Date =? fromString (show y)
  --Just y -> Date =? (Value $ encode y)
  Nothing -> anything
