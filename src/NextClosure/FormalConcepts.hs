module NextClosure.FormalConcepts where

import FormalContext.FormalContext2
import NextClosure.NextClosure
import Util


type Intent m = [m]

data FormalConcept g m
  = FormalConcept { ext :: [g]
                  , int :: [m]
                  } deriving (Eq)
                
instance (Show g,Show m) => Show (FormalConcept g m) where
  show (FormalConcept e i)
    = show e ++ "x" ++ show i

toFormalConcept :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                => FormalContext2 g m -> Intent m -> FormalConcept g m
nextIntent      :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                => FormalContext2 g m -> Iterator (Intent m)
intents         :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                => FormalContext2 g m -> [Intent m]
formalConcepts  :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                => FormalContext2 g m -> [FormalConcept g m]

toFormalConcept cxt int'
  = FormalConcept (extent cxt int') int'
    
nextIntent cxt
  = nextClosure cxt $ attClosure cxt

intents cxt@(FormalContext2 _ cod _ _)
  = iterateWhile (not.(===cod)) (nextIntent cxt) (attClosure cxt [])
  
formalConcepts cxt
  = map (toFormalConcept cxt) (intents cxt)

