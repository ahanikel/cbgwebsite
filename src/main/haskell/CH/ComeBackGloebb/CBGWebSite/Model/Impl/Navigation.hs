{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Navigation () where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           Data.Foldable
import           Data.Traversable

data Navigation a = Navigation { navSelf     :: a
                               , navParent   :: a
                               , navSiblings ::[a]
                               , navChildren :: [a]
                               }
                    deriving (Functor, Foldable, Traversable)
