{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module FormalContext.FormalContext where

import FormalContext.Matrices
import Util
import Data.List
import Data.Maybe
import qualified Data.Array.Repa as R


data FormalContext g m = (Ord g, Ord m, Show g, Show m, Eq g, Eq m) 
                       => FormalContext { domain   :: [g]
                                        , codomain :: [m]
                                        , matrix   :: BooleanMatrix
                                        } 
                            
instance Show (FormalContext g m) where
  show (FormalContext dom cod mat)
    = "FormalContext"
      ++ "\n with Domain: " ++ show dom
      ++ "\n with Codomain: " ++ show cod
      ++ "\n with Incidences: " ++ show mat

objIndex   :: FormalContext g m -> g -> Int
attIndex   :: FormalContext g m -> m -> Int
attExtent  :: FormalContext g m -> m -> [g]
objIntent  :: FormalContext g m -> g -> [m]
extent     :: FormalContext g m -> [m] -> [g]
intent     :: FormalContext g m -> [g] -> [m]
attClosure :: FormalContext g m -> ClosureOperator m
objClosure :: FormalContext g m -> ClosureOperator g

objIndex (FormalContext dom _ _) obj
  = fromJust $ elemIndex obj dom

attIndex (FormalContext _ cod _) att
  = fromJust $ elemIndex att cod

attExtent cxt@(FormalContext dom _ mat) att
  = filter (\obj -> col R.! (R.Z R.:. objIndex cxt obj)) dom
    where col = getColumn (attIndex cxt att) mat

objIntent cxt@(FormalContext _ cod mat) obj
  = filter (\att -> row R.! (R.Z R.:. attIndex cxt att)) cod
    where row = getRow (objIndex cxt obj) mat

extent cxt@(FormalContext dom _ mat) atts
  = filter (\obj -> colsAnd R.! (R.Z R.:. objIndex cxt obj)) dom
    where colsAnd = foldColumnsByAnd (map (attIndex cxt) atts) mat
    
intent cxt@(FormalContext _ cod mat) objs
  = filter (\att -> rowsAnd R.! (R.Z R.:. attIndex cxt att)) cod
    where rowsAnd = foldRowsByAnd (map (objIndex cxt) objs) mat

attClosure cxt atts
  = intent cxt $ extent cxt atts
  
objClosure cxt objs
  = extent cxt $ intent cxt objs

