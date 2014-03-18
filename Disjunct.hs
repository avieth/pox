module Disjunct (
    Disjunct
  , getConjuncts
  , disjunctSingle
  , disjunctOr
  , disjunctAnd
  ) where

import Conjunct

-- | A disjunction of conjunctions.
data Disjunct = MkDisjunct {
    unDisjunct :: [Conjunct]
  } deriving (Show)

getConjuncts = unDisjunct

disjunctSingle :: Conjunct -> Disjunct
disjunctSingle x = MkDisjunct [x]

-- | Disjunction of disjunctions is just list concatenation.
disjunctOr :: Disjunct -> Disjunct -> Disjunct
disjunctOr a b = MkDisjunct $ (unDisjunct a) ++ (unDisjunct b)

-- | Conjunction of disjunctions is the distribution of each conjunction
--   in the left Disjunct over the right Disjunct.
disjunctAnd :: Disjunct -> Disjunct -> Disjunct
disjunctAnd a b = MkDisjunct $ distribute (unDisjunct a) (unDisjunct b)

-- | We perform distribution using the standard list monad: distribute each
--   single conjunct over the list of conjuncts, then concatenate.
distribute :: [Conjunct] -> [Conjunct] -> [Conjunct]
distribute [] ys = ys
distribute xs ys = xs >>= (\x -> singleDistribute x ys)

-- Distribute a single Conjunct over a list of Conjuncts.
singleDistribute :: Conjunct -> [Conjunct] -> [Conjunct]
singleDistribute x xs = map (conjunctAnd x) xs
