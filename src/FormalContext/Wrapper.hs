module FormalContext.Wrapper where

import FormalContext.FormalContext
import FormalContext.FormalContext2
import Data.Map
import Data.List

toFormalContext2 :: FormalContext g m -> FormalContext2 g m
toFormalContext2 cxt@(FormalContext dom cod mat)
  = FormalContext2 (reverse $ sort dom) (reverse $ sort cod) rows cols
    where rows = fromList $ [(obj, FormalContext.FormalContext.objIntent cxt obj) | obj <- dom]
          cols = fromList $ [(att, FormalContext.FormalContext.attExtent cxt att) | att <- cod]

