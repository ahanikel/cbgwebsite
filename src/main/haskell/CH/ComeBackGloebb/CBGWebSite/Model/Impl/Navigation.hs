{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Navigation (getNavigation, Navigation(..), NavigationEntry(..), getTrail) where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Utils
import           Control.Monad                                     (liftM)
import           Control.Monad.Trans.Either                        (runEitherT)
import qualified Data.ByteString.Lazy.UTF8                         as UL8
import           Data.Foldable
import           Data.Traversable
import qualified Data.Tree                                         as T

data Navigation a = Navigation { navParent   :: Maybe a
                               , navSiblings :: [a]
                               , navTree     :: T.Tree a
                               }
                    deriving (Show, Functor, Foldable, Traversable)

data NavigationEntry = NavigationEntry { neTitle :: String
                                       , neRank  :: Int
                                       , neURL   :: URL
                                       }
                       deriving (Show)

instance Eq NavigationEntry where
  x == y = neURL x == neURL y

instance Ord NavigationEntry where
  x `compare` y = neRank x `compare` neRank y

getTree :: Node -> Int -> RepositoryContext (T.Tree Node)
getTree node subLevels | subLevels == 0 = return $ T.Node node []
getTree node subLevels | subLevels >  0 = do
  childNodes <- getChildNodes node
  res <- mapM (`getTree` (subLevels - 1)) childNodes
  return $ T.Node node res

-- exported
getNavigation :: Repository -> URL -> Int -> RepositoryContext (Navigation NavigationEntry)
getNavigation repo path subLevels = do
  self     <- getNode repo path
  parent   <- getParentNode self
  siblings <- getSiblings self
  children <- getTree self subLevels
  let navi =  Navigation parent siblings children
  check $ mapM naviEntryFromNode navi

naviEntryFromNode :: Node -> IO NavigationEntry
naviEntryFromNode n = do
  title <- getTitleOrNodeName n
  rank  <- getRankOrZero n
  let url = node_path n
  return $ NavigationEntry title rank url

getTitleOrNodeName :: Node -> IO String
getTitleOrNodeName n = do
  eprop <- runEitherT $ liftM UL8.toString $ getProperty n "title"
  return $ either (const $ node_name n) id eprop

getRankOrZero :: Node -> IO Int
getRankOrZero n = do
  eprop <- runEitherT $ liftM ((read :: String -> Int) . UL8.toString) $ getProperty n "rank"
  return $ either (const 0) id eprop

-- exported
getTrail :: Node -> RepositoryContext [NavigationEntry]
getTrail node = do
  ns <- foldrM appendToNodes [node] $ node_path node
  mapM (check . naviEntryFromNode) ns
  where
    appendToNodes :: PathComponent -> [Node] -> RepositoryContext [Node]
    appendToNodes _ (n : ns) = do mp <- getParentNode n
                                  case mp of
                                    Nothing -> return (n : ns)
                                    Just p  -> return (p : n : ns)
    appendToNodes _ _ = undefined
