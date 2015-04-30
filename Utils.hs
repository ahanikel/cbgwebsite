module Utils (check) where

import Control.Monad.Error (ErrorT(..))
import Control.Exception (IOException, try)

-- from https://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html
check :: IO a -> ErrorT IOException IO a
check io = ErrorT (try io)

