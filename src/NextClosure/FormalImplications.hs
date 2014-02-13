module NextClosure.FormalImplications where

import FormalContext.FormalContext2
import NextClosure.NextClosure
import Util
import Data.List


type PseudoIntent m = [m]

data FormalImplication m
  = FormalImplication { premise    :: [m]
                      , conclusion :: [m]
                      } deriving (Eq)
                
instance (Show m) => Show (FormalImplication m) where
  show (FormalImplication p c)
    = show p ++ "=>" ++ show c

toFormalImplication   :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                      => FormalContext2 g m -> PseudoIntent m -> FormalImplication m
(??)                  :: (Ord m, Show m, Eq m)
                      => [m] -> FormalImplication m -> Bool
(???)                 :: (Ord m, Show m, Eq m)
                      => [m] -> [FormalImplication m] -> Bool
formalImplications    :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                      => FormalContext2 g m -> [FormalImplication m]
nextFormalImplication :: (Ord g, Ord m, Show g, Show m, Eq g, Eq m)
                      => FormalContext2 g m -> Iterator (PseudoIntent m,[FormalImplication m])
implicationalClosure  :: (Ord m, Show m, Eq m)
                      => [FormalImplication m] -> ClosureOperator m

toFormalImplication cxt pint
  = FormalImplication pint (attClosure cxt pint \\ pint)

-- this method tests, whether an attribute list respects an implication
x ?? (FormalImplication p c)
  = not (x <<= p) || x <<= c

-- this method tests, whether an attribute list respects all implications in a list
x ??? impls
  = all (x ??) impls
  
formalImplications cxt
  = snd (nextFormalImplication cxt >$$> ([],[toFormalImplication cxt []]))

nextFormalImplication _ (_,[])
  = undefined
nextFormalImplication cxt@(FormalContext2 _ cod _ _) (p,impls)
  | p === cod = (p,impls)
  | otherwise = (p',null c' ? impls $ imp:impls)
    where p' = nextClosure cxt (implicationalClosure impls) p
          imp@(FormalImplication _ c') = toFormalImplication cxt p'

implicationalClosure impls
  = (close >$$>)
    where close ms
            = nub $ respect impls ++ ms
              where respect []
                      = []
                    respect (FormalImplication p c : impls')
                      | p <<= ms  = c ++ respect impls'
                      | otherwise = respect impls'

