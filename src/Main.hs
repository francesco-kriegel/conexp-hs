module Main where

import FormalContext.Wrapper
import IO.BurmeisterReader
import IO.AttributeExplorationWriter
import IO.Files
import NextClosure.AttributeExploration
import Data.Time
import Data.String.Utils
import Control.Monad (liftM)
import Control.Parallel

main                  :: IO ()
attributeExploration  :: IO ()
attributeExploration' :: FilePath -> IO ()

main
  = undefined

attributeExploration
  = do {            putStrLn "Input File?"
       ; input'  <- readString
       ;            putStrLn "Output File?"
       ; output' <- readString >>= resolveFilePath
       ; (ZonedTime (LocalTime _ time) _) <- getZonedTime
       ;            putStrLn $ show time ++ " Reading " ++ show input' ++ "..."
       ; cxt     <- readBurmeister input'
       ; (ZonedTime (LocalTime _ time') _) <- getZonedTime
       ;            putStrLn $ show time' ++ " Reading done."
       --;            print cxt
       ; cxt'    <- return $ toFormalContext2 cxt
       ; (ZonedTime (LocalTime _ time'') _) <- getZonedTime
       ;            putStrLn $ show time'' ++ " Conversion done. Now exploring..."
       ; ae      <- exploreIO cxt'
       ; (ZonedTime (LocalTime _ time''') _) <- getZonedTime
       ;            putStrLn $ show time''' ++ " Exploration done. Now storing results..."
       ;            writeAttributeExploration output' ae
       ; (ZonedTime (LocalTime _ time'''') _) <- getZonedTime
       ;            putStrLn $ show time'''' ++ " Storing done."
       }

attributeExploration' input'
  = do { output' <- resolveFilePath $ replace ".cxt" ".aexp" input'
       ; (ZonedTime (LocalTime _ time) _) <- getZonedTime
       ;            putStrLn $ show time ++ " Reading " ++ show input' ++ "..."
       ; cxt     <- readBurmeister input'
       ; (ZonedTime (LocalTime _ time') _) <- getZonedTime
       ;            putStrLn $ show time' ++ " Reading done. Now converting..."
       --;            print cxt
       ; cxt'    <- return $ toFormalContext2 cxt
       ; (ZonedTime (LocalTime _ time'') _) <- getZonedTime
       ;            putStrLn $ show time'' ++ " Conversion done. Now exploring..."
       ; aexp    <- exploreIO cxt'
       ; (ZonedTime (LocalTime _ time''') _) <- getZonedTime
       ;            putStrLn $ show time''' ++ " Exploration done. Now storing results to " ++ show output' ++ "..."
       ;            writeAttributeExploration output' aexp
       ; (ZonedTime (LocalTime _ time'''') _) <- getZonedTime
       ;            putStrLn $ show time'''' ++ " Storing done."
       }

readString :: IO String
readString = readLn


seqTest :: IO ()
seqTest
  = do
      time <- getCurrentTime
      putStrLn $ foo [1..100000000] [1..100000000]
      time' <- getCurrentTime
      putStrLn $ show (diffUTCTime time' time)

parTest :: IO ()
parTest
  = do
      time <- getCurrentTime
      putStrLn $ par' foo [1..100000000] [1..100000000]
      time' <- getCurrentTime
      putStrLn $ show (diffUTCTime time' time)

foo :: [a] -> [b] -> String
foo x y = show (length x) ++ ", " ++ show (length y)
      
par' :: (a -> b -> c) -> a -> b -> c
par' f x y = y `par` (f x y)


