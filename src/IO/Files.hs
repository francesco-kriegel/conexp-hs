module IO.Files where

import Control.Monad
import Data.String.Utils
import System.Directory
import Control.Arrow


writeStrings :: FilePath -> [String] -> IO ()
readStrings  :: FilePath -> IO [String]

writeStrings file content
  = writeFile file (unlines content)

readStrings
  = liftM (fmap (lines <<< replace "\r" "")) readFile


isNotParentOrCurrentDirectory :: FilePath -> IO Bool
resolveFilePath               :: FilePath -> IO FilePath

isNotParentOrCurrentDirectory filePath
  = return $ filePath `notElem` [".",".."]

resolveFilePath f
  = liftM (++ f) getCurrentDirectory


subFilesCur :: IO [FilePath]
subFilesRel :: FilePath -> IO [FilePath]
subFilesAbs :: FilePath -> IO [FilePath]
subDirsCur  :: IO [FilePath]
subDirsRel  :: FilePath -> IO [FilePath]
subDirsAbs  :: FilePath -> IO [FilePath]

subFilesCur
  = subFilesRel ""

subFilesRel path
  = resolveFilePath path >>= subFilesAbs

subFilesAbs path
  = getDirectoryContents path
    >>= filterM isNotParentOrCurrentDirectory

subDirsCur
  = subDirsRel ""

subDirsRel path
  = resolveFilePath path >>= subDirsAbs
                
subDirsAbs path
  = subFilesAbs path
    >>= filterM doesDirectoryExist
--  >>= filterM isNotParentOrCurrentDirectory

