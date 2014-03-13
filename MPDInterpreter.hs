module MPDInterpreter (
    evalComprehension
  ) where

import Control.Monad
import Data.String (fromString)

import Network.MPD

import Comprehension hiding ((<&>))
import Conjunct
import Disjunct

-- | Produce a list of Songs from a Comprehension, inside some MonadMPD.
evalComprehension :: (MonadMPD m) => Comprehension -> m [Song]
evalComprehension = (liftM concat) . ((flip forM) find) . queriesFromComprehension

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
  Just y -> Date =? fromString (show y)
  Nothing -> anything
