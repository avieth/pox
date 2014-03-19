module Conjunct (
    Conjunct(..)
  , getArtist
  , getAlbum
  , getTitle
  , getDate
  , mkArtist
  , mkAlbum
  , mkTitle
  , mkDate
  , conjunctAnd
  ) where

import Types

data Conjunct = MkConjunct {
    _artist :: Maybe Artist
  , _album :: Maybe Album
  , _title :: Maybe Title
  , _date :: Maybe Date
  } deriving (Show)

getArtist = _artist
getAlbum = _album
getTitle = _title
getDate = _date

mkArtist :: Artist -> Conjunct
mkArtist artist = MkConjunct {
    _artist = Just artist
  , _album = Nothing
  , _title = Nothing
  , _date = Nothing
  }

mkAlbum :: Album -> Conjunct
mkAlbum album = MkConjunct {
    _artist = Nothing
  , _album = Just album
  , _title = Nothing
  , _date = Nothing
  }

mkTitle :: Title -> Conjunct
mkTitle title = MkConjunct {
    _artist = Nothing
  , _album = Nothing
  , _title = Just title
  , _date = Nothing
  }

mkDate :: Date -> Conjunct
mkDate date = MkConjunct {
    _artist = Nothing
  , _album = Nothing
  , _title = Nothing
  , _date = Just date
  }

-- Conjunct is not a lattice, because some don't make sense. For instance:
--
--   conjunctArtist "Radiohead" `meet` conjunctArtist "Blur"
--
-- is nonsense... well, we could just call that the bottom, but I'd rather
-- rule it out!
--
conjunctAnd :: Conjunct -> Conjunct -> Conjunct
conjunctAnd a b = MkConjunct {
  _artist = conjunctFieldMeet _artist a b
, _album = conjunctFieldMeet _album a b
, _title = conjunctFieldMeet _title a b
  , _date = conjunctFieldMeet _date a b
  }

conjunctFieldMeet :: (Conjunct -> Maybe a) -> Conjunct -> Conjunct -> Maybe a
conjunctFieldMeet f a b = case f a of
  Nothing -> f b
  Just a -> case f b of
    Nothing -> Just a
    -- TODO could just favour the left or right... no, best to be honest about
    -- the error I think.
    Just b -> error "Conjunction does not make sense!"
