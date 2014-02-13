module IO.StringState where

import Control.Monad.State


type StringState = State String (Maybe Bool)

write   :: FilePath -> StringState -> IO ()
eval    :: StringState -> String
newLine :: StringState
append  :: String -> StringState
apply   :: (String -> String) -> StringState

write f s = writeFile f (eval s)
eval s    = execState s ""
newLine   = append "\n"
append s  = apply (++s)
apply f   = get >>= put.f >> return (Just True)

