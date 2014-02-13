{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module IO.BurmeisterReader where

import FormalContext.Matrices
import FormalContext.FormalContext
import FormalContext.FormalContexts
import IO.Files
import Data.List
import Data.String.Utils


type Burmeister = ([String],FormalContext String String)

readBurmeister  :: FilePath -> IO (FormalContext String String)
initBurmeister  :: FilePath -> IO Burmeister
checkBurmeister :: Burmeister -> IO Burmeister
readObjects     :: Burmeister -> IO Burmeister
readAttributes  :: Burmeister -> IO Burmeister
readIncidences  :: Burmeister -> IO Burmeister
done            :: Burmeister -> IO (FormalContext String String)
readCross       :: Char -> Bool

readBurmeister f = initBurmeister f
                   >>= checkBurmeister
                   >>= readObjects
                   >>= readAttributes
                   >>= readIncidences
                   >>= done

initBurmeister f
  = do l <- resolveFilePath f >>= readStrings
       return (l,emptyContext)

checkBurmeister (l,cxt)
  = if 'B' `elem` strip (head l) && strip (l!!1)=="" && strip (l!!4)==""
    then return (l,cxt)
    else error $ "This is not a file in Burmeister formatting:\n" ++ show l

readObjects (l,FormalContext _ cod mat)
  = let nObj = read (l!!2) :: Int
        dom' = take nObj (drop 5 l) :: [String]
        in return (l,FormalContext dom' cod mat)

readAttributes (l,FormalContext dom _ mat)
  = let nObj = length dom :: Int
        nAtt = read (l!!3) :: Int
        cod' = take nAtt (drop (5+nObj) l) :: [String]
        in return (l,FormalContext dom cod' mat)

readIncidences (l,FormalContext dom cod _)
  = let rows = length dom
        cols = length cod
        crosses = take rows (drop (5+rows+cols) l)
        mat' = toBooleanMatrix rows cols (map readCross (concat crosses))
        in return (l,FormalContext dom cod mat')

done (_,cxt)
  = return cxt

readCross 'X' = True
readCross 'x' = True
readCross '.' = False
readCross _   = error "No Burmeister formatting."

