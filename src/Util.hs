module Util where

import Control.Monad.State


type Iterator a        = a -> a
type ClosureOperator a = Iterator [a]

(?)    :: Bool -> a -> a -> a
(<<=)  :: (Eq m) => [m] -> [m] -> Bool
(===)  :: (Eq m) => [m] -> [m] -> Bool
(***)  :: [a] -> [b] -> [(a,b)]
(>$$>) :: (Eq a) => Iterator a -> a -> a

-- if c then x else y == (c ? x $ y)
True  ? x = const x
False ? _ = id

[] <<= _      = True
(_:_) <<= []  = False
(m:ms) <<= ns = m `elem` ns && ms <<= ns

ms === ns = ms <<= ns && ns <<= ms

(***) = liftM2 (,)

f >$$> x
  | x == y    = x
  | otherwise = f >$$> y
    where y = f x


iterateWhile :: (a -> Bool) -> Iterator a -> a -> [a]
whileM       :: (a -> Bool) -> State s a -> State s a
whileM2      :: (a -> Bool) -> (a -> State s a) -> a -> State s a

iterateWhile c f i
  | c i       = let j = f i in i:iterateWhile c f j
  | otherwise = [i]

whileM condition state' = do
        x <- state'
        if condition x
          then whileM condition state'
          else return x

whileM2 condition state' value
  = do
      value' <- state' value
      if condition value'
        then whileM2 condition state' value'
        else return value'

