{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Navigation () where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           Control.Monad                                     (liftM)
import           Data.Foldable
import           Data.Traversable
import qualified Data.Tree                                         as T

data Navigation a = Navigation { navSelf     :: a
                               , navParent   :: a
                               , navSiblings :: [a]
                               , navChildren :: T.Tree a
                               }
                    deriving (Functor, Foldable, Traversable)

getChildren :: Node -> Int -> RepositoryContext (T.Tree Node)
getChildren node subLevels | subLevels == 0 = return $ T.Node node []
getChildren node subLevels | subLevels >  0 = do
  childNodes <- getChildNodes node
  res <- mapM (flip getChildren (subLevels - 1)) childNodes
  return $ T.Node node res

-- exported
getNavigationFor :: Repository -> String -> Int -> RepositoryContext (Navigation Node)
getNavigationFor repo path subLevels = do
  self     <- getNode repo (urlFromString path)
  parent   <- getParentNode self
  siblings <- getSiblings self
  children <- getChildren self subLevels
  return $ Navigation self parent siblings children
