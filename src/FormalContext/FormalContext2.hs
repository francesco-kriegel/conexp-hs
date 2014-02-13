{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module FormalContext.FormalContext2 where

import Util
import Data.List
import Data.Map
import Data.Set


data FormalContext2 g m = (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                        => FormalContext2 { domain   :: [g]
                                          , codomain :: [m]
                                          , rows     :: Map g [m]
                                          , cols     :: Map m [g]
                                          } 
                            
instance Show (FormalContext2 g m) where
  show (FormalContext2 dom cod _ _)
    = "FormalContext"
      ++ "\n with Domain: " ++ show dom
      ++ "\n with Codomain: " ++ show cod

attExtent  :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m) => FormalContext2 g m ->  m  -> [g]
objIntent  :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m) => FormalContext2 g m ->  g  -> [m]
extent     :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m) => FormalContext2 g m -> [m] -> [g]
intent     :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m) => FormalContext2 g m -> [g] -> [m]
attClosure :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m) => FormalContext2 g m -> ClosureOperator m
objClosure :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m) => FormalContext2 g m -> ClosureOperator g

attExtent cxt att = cols cxt ! att
objIntent cxt obj = rows cxt ! obj

extent (FormalContext2 dom _ _ _) [] = dom
extent cxt (att:[])                  = attExtent cxt att
extent cxt (att:atts)                = attExtent cxt att `intersect` extent cxt atts

intent (FormalContext2 _ cod _ _) [] = cod
intent cxt (obj:[])                  = objIntent cxt obj
intent cxt (obj:objs)                = objIntent cxt obj `intersect` intent cxt objs

attClosure cxt = intent cxt . extent cxt
objClosure cxt = extent cxt . intent cxt

