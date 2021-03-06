module CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Utils (check) where

import           Control.Monad.Trans.Either (EitherT (..))
import           System.IO.Error            (tryIOError)

-- from https://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html
check :: IO a -> EitherT IOError IO a
check io = EitherT (tryIOError io)
