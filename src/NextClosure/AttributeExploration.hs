module NextClosure.AttributeExploration where

import FormalContext.FormalContext2
import NextClosure.NextClosure
import NextClosure.FormalConcepts
import NextClosure.FormalImplications
import Util
import Data.Time


data AE g m
  = AE { implications  :: [FormalImplication m]
       , concepts      :: [FormalConcept g m]
       } deriving (Eq,Show)

data AEResult g m
  = AEResult { pseudoIntent :: PseudoIntent m
             , result       :: Either (FormalImplication m) (FormalConcept g m)
             } deriving (Eq)

data AEState g m
  = AEState { step :: Int
            , ae  :: AE g m
            , aer :: AEResult g m
            } deriving (Eq)
    
explore :: (Show g,Show m,Eq g,Eq m,Ord g,Ord m) => FormalContext2 g m -> AE g m
explore cxt
  = ae
    where AEState _ ae _ = runState cxt

runState :: (Eq g,Eq m,Show m,Show g,Ord g,Ord m) => FormalContext2 g m -> AEState g m
runState cxt
  | [] == attClosure cxt [] = nextState cxt >$$> (AEState 1 (AE [] [cpt]) (AEResult [] (Right cpt)))
  | otherwise               = nextState cxt >$$> (AEState 1 (AE [impl] []) (AEResult [] (Left impl)))
    where cpt  = toFormalConcept cxt []
          impl = toFormalImplication cxt []

nextState :: FormalContext2 g m -> Iterator (AEState g m)
nextState cxt@(FormalContext2 _ cod _ _) y@(AEState i (AE impls cpts) (AEResult p x))
  | p === cod = y
  | otherwise = AEState (i+1) (AE impls' cpts') (AEResult p' x')
    where p'     = nextClosure cxt (implicationalClosure impls) p
          imp@(FormalImplication _ c') = toFormalImplication cxt p'
          cpt    = toFormalConcept cxt p'
          x'     = null c' ? (Right cpt) $ (Left imp)
          impls' = null c' ? impls $ imp:impls
          cpts'  = null c' ? (cpt:cpts) $ cpts
          
silentIO :: (Show g,Show m,Eq g,Eq m,Ord g,Ord m) => FormalContext2 g m -> IO String
silentIO cxt
  = do
      exploreIO cxt
      return "done."

exploreIO :: (Show g,Show m,Eq g,Eq m,Ord g,Ord m) => FormalContext2 g m -> IO (AE g m)
exploreIO cxt
  = do
      state@(AEState _ _ aer@(AEResult _ result')) <- initIO cxt
      (AEState _ ae _) <- runIO cxt state
      return ae

runIO :: (Show g,Show m,Eq g,Eq m) => FormalContext2 g m -> AEState g m -> IO (AEState g m)
runIO cxt@(FormalContext2 _ cod _ _) state@(AEState _ _ aer@(AEResult pint _))
  = if (pint === cod)
      then return state
      else do
             state' <- nextIO cxt state
             runIO cxt state'

nextIO :: (Show g,Show m,Eq g,Eq m) => FormalContext2 g m -> AEState g m -> IO (AEState g m)
nextIO cxt state@(AEState i _ aer@(AEResult _ result'))
  = do
      (ZonedTime (LocalTime _ time) _) <- getZonedTime
      putStrLn $ show time ++ " " ++ show i ++ " " ++ show' result'
      return $ nextState cxt state

initIO :: (Eq g,Eq m,Show g, Show m,Ord g,Ord m) => FormalContext2 g m -> IO (AEState g m)
initIO cxt
  | [] == attClosure cxt [] = return (AEState 1 (AE [] [cpt]) (AEResult [] (Right cpt)))
  | otherwise               = return (AEState 1 (AE [impl] []) (AEResult [] (Left impl)))
    where cpt  = toFormalConcept cxt []
          impl = toFormalImplication cxt []

show' :: (Show g,Show m) => Either (FormalImplication m) (FormalConcept g m) -> String
show' (Left x)  = show x ++ "\n"--"Implication:\n" ++ 
show' (Right x) = ""--"\n\n Formal Concept:\n" ++ show x ++ "\n"

toEither :: (Ord g,Eq g,Ord m,Eq m,Show g,Show m)
         => FormalContext2 g m -> PseudoIntent m -> Either (FormalImplication m) (FormalConcept g m)
toEither cxt pint
  | pint == attClosure cxt pint = Right $ toFormalConcept     cxt pint
  | otherwise                   = Left  $ toFormalImplication cxt pint

