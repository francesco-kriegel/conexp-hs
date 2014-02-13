{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module FormalContext.Matrices where

import qualified Data.Array.Repa as R


type BooleanVector = R.Array R.U R.DIM1 Bool
type BooleanMatrix = R.Array R.U R.DIM2 Bool
         
getColumn        ::  Int  -> BooleanMatrix -> BooleanVector
getRow           ::  Int  -> BooleanMatrix -> BooleanVector
foldColumnsByAnd :: [Int] -> BooleanMatrix -> BooleanVector
foldRowsByAnd    :: [Int] -> BooleanMatrix -> BooleanVector
rowCount         :: BooleanMatrix -> Int
colCount         :: BooleanMatrix -> Int
newMatrix        :: BooleanMatrix
toBooleanMatrix  :: Int -> Int -> [Bool] -> BooleanMatrix

getColumn col mat
  = R.computeUnboxedS $ R.fromFunction (R.Z R.:. rowCount mat) (\(R.Z R.:. r) -> mat R.! (R.Z R.:. r R.:. col))
  
getRow row mat
  = R.computeUnboxedS $ R.fromFunction (R.Z R.:. colCount mat) (\(R.Z R.:. c) -> mat R.! (R.Z R.:. row R.:. c))
  
foldColumnsByAnd cols mat
  = R.computeUnboxedS $ R.fromFunction
    (R.Z R.:. rowCount mat) (\(R.Z R.:. r) -> foldl (\b col -> b && mat R.! (R.Z R.:. r R.:. col)) True cols)
    
foldRowsByAnd rows mat
  = R.computeUnboxedS $ R.fromFunction
    (R.Z R.:. colCount mat) (\(R.Z R.:. c) -> foldl (\b row -> b && mat R.! (R.Z R.:. row R.:. c)) True rows)

rowCount
  = last.R.listOfShape.R.extent

colCount
  = head.R.listOfShape.R.extent

newMatrix
  = toBooleanMatrix 0 0 []

toBooleanMatrix r c
  = R.fromListUnboxed (R.Z R.:. r R.:. c)

