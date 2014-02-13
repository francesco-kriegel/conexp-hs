module FormalContext.FormalContexts where

import FormalContext.FormalContext
import FormalContext.Matrices
import Data.List
import Util


emptyContext       :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                   => FormalContext g m
fromPredicate      :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                   => (g -> m -> Bool) -> [g] -> [m] -> FormalContext g m    
booleanScale       :: Int -> FormalContext [Int] [Int]    
nominalScale       :: Int -> FormalContext Int Int
ordinalScale       :: Int -> FormalContext Int Int    
contranominalScale :: Int -> FormalContext Int Int

emptyContext
  = FormalContext [] [] newMatrix

fromPredicate predicate dom cod
  = FormalContext dom cod mat
    where mat = toBooleanMatrix (length dom) (length cod) (map (uncurry (flip predicate)) (cod *** dom))

booleanScale n
  = fromPredicate (<<=) d d
    where d = subsequences [1..n]

nominalScale n
  = fromPredicate (==) d d
    where d = [1..n]

ordinalScale n
  = fromPredicate (<=) d d
    where d = [1..n]

contranominalScale n
  = fromPredicate (/=) d d
    where d = [1..n]

