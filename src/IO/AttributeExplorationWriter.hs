module IO.AttributeExplorationWriter where

import NextClosure.AttributeExploration
import IO.StringState


writeAttributeExploration :: (Show g,Show m)
                          => FilePath -> AE g m -> IO ()
writeAttributeExploration file (AE impls cpts)
  = write file $ writeHeader >> writeImplications >> writeConcepts
    where writeHeader       :: StringState
          writeImplications :: StringState
          writeConcepts     :: StringState
          writeHeader       = append $ "\n"
          writeImplications = append $ unlines (map show impls) ++ "\n"
          writeConcepts     = append $ unlines (map show cpts) ++ "\n"

