module NextClosure.NextClosure where

import FormalContext.FormalContext2
import Util
import Data.List
import Data.Maybe
import Control.Arrow
import Control.Parallel


(<++>)       :: (Ord m) => [m] -> m -> [m]
nextClosure  :: (Ord m) => FormalContext2 g m -> ClosureOperator m -> Iterator [m]
nextClosureM :: (Ord m) => FormalContext2 g m -> ClosureOperator m -> Iterator [m]
nextClosureS :: (Ord m) => FormalContext2 g m -> ClosureOperator m -> Iterator [m]
nextClosureP :: (Ord m) => FormalContext2 g m -> ClosureOperator m -> Iterator [m]
nextClosureP2:: (Ord m) => FormalContext2 g m -> ClosureOperator m -> Iterator [m]
nextClosureP3:: (Ord m) => FormalContext2 g m -> ClosureOperator m -> Iterator [m]
lexSmallerBy :: (Ord m) => m -> [m] -> [m] -> Bool

-- this method computes the A plus g set for NextClosure algorithm
-- but does not compute its closure
atts <++> att
  = att:filter (< att) atts
  
nextClosure = nextClosureM

nextClosureM (FormalContext2 _ cod _ _) clop atts
  = clop $ atts <++> maxatt
    where maxatt = maximum [att | att <- cod, lexSmallerBy att atts (clop $ atts <++> att)]

nextClosureS (FormalContext2 _ cod _ _) clop atts
  = clop $ atts <++> maxatt
    where maxatt = findmax cod 
          findmax [] = error "unable to compute next closure"
          findmax (att:cod')
            | lexSmallerBy att atts (clop $ atts <++> att) = att
            | otherwise = findmax cod'

nextClosureP (FormalContext2 _ cod _ _) clop atts
  = clop $ atts <++> maxatt
    where maxatt = findmaxp cod
          findmaxp [] = error "unable to compute next closure"
          findmaxp (att:[]) = att
          findmaxp (att:cod')
            | (findmaxp cod') `par` (lexSmallerBy att atts (clop $ atts <++> att)) = att
            | otherwise = findmaxp cod'

nextClosureP2 (FormalContext2 _ cod _ _) clop atts
  = clop $ atts <++> maxatt
    where maxatt = findmaxp cod
          findmaxp [] = error "unable to compute next closure"
          findmaxp (att:[]) = att
          findmaxp (att:att':cod')
            | (lexSmallerBy att' atts (clop $ atts <++> att'))
              `par` (lexSmallerBy att atts (clop $ atts <++> att)) = att
            | lexSmallerBy att' atts (clop $ atts <++> att') = att'
            | otherwise = findmaxp cod'

nextClosureP3 (FormalContext2 _ cod _ _) clop atts
  = head [clos | att <- cod, let clos = clop $ atts <++> att, lexSmallerBy att atts clos]

nextClosureP4 (FormalContext2 _ cod _ _) clop atts
  = undefined

-- parallelization
  -- compute (clop $ atts <++> att) in parallel for all att <- cod
  -- -- start computation with biggest element,
  -- -- and then continue in descending order
  -- -- return first (and thus biggest) att

lexSmallerBy m ms ns
  = m `elem` ns && m `notElem` ms && filter (< m) ms === filter (< m) ns

  