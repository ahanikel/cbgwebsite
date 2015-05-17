module Repository
    ( Repository (..)
    , openRepository
    , RepositoryContext
    , Node (..)
    , getNode
    , getParentNode
    , getChildNodeNames
    , getChildNode
    , getChildNodes
    , Property (..)
    , getProperty
    , Value (..)
    , URL
    , urlFromString
    , urlToString
    , PathComponent
    , pathCompFromString
    , isRootNode
    )
where

import Utils
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import Control.Monad (filterM)
import System.FilePath (normalise, splitDirectories, (</>))
import Control.Monad.Trans.Either (EitherT, left)
import System.IO.Error (userError)
import Data.List (intercalate)

-- exported
data Repository = Repository { root :: FilePath
                             }
    deriving (Read, Show, Eq)

-- exported
openRepository :: FilePath -> Repository
openRepository = Repository

-- exported
type PathComponent = String

-- exported
pathCompFromString :: FilePath -> PathComponent 
pathCompFromString = id

-- exported
type URL = [PathComponent]

-- exported
urlFromString :: String -> URL
urlFromString ""      = []
urlFromString ('/':s) = urlFromString s
urlFromString s       = map pathCompFromString $ splitDirectories $ normalise s

-- exported
urlToString :: URL -> String
urlToString = ('/' :) . (intercalate "/")

-- exported
data Value = StringValue String
           | NumberValue Integer
           | BooleanValue Bool
    deriving (Read, Eq)

instance Show Value where
    show (StringValue s)  = s
    show (NumberValue i)  = show i
    show (BooleanValue b) = show b

-- exported
data Property = Property { prop_name :: String
                         , prop_value :: Value
                         }
    deriving (Read, Show, Eq)

-- exported
data Node = Node { node_name        :: String
                 , node_path        :: URL
                 , node_props       :: [Property]
                 , node_repo        :: Repository
                 }
    deriving (Read, Show, Eq)

-- exported
type RepositoryContext a = EitherT IOError IO a

readProperties :: FilePath -> IO [Property]
readProperties path = readFiles path      >>=
                      filterM filterFiles >>=
                      mapM readProperty
    where
          -- return a list of pairs (fileName, completePath), e.g. ("text.md", "content/welcome/text.md")
          readFiles :: FilePath -> IO [(FilePath, FilePath)]
          readFiles path            = getDirectoryContents path >>= return . map (\n -> (n, path </> n))
          filterFiles (name, path)  = doesFileExist path
          readProperty (name, path) = readFile path >>= return . Property name . StringValue

-- exported
getNode :: Repository -> URL -> RepositoryContext Node
getNode repo url = do let name       = case url of [] -> "/"
                                                   _  -> last url
                          filePath   = intercalate "/" (root repo : url)
                      props <- check (readProperties filePath)
                      return $ Node name url props repo

-- exported
getParentNode :: Node -> RepositoryContext Node
getParentNode node | node_name node == "/" = left $ userError "Root has no parent"
getParentNode node | otherwise = do let path     = init $ node_path node
                                    getNode (node_repo node) path

-- exported
getChildNodeNames :: Node -> RepositoryContext [String]
getChildNodeNames node = do let path       = node_path node
                                url'       = '/' : foldr (</>) "" path
                                repo       = node_repo node
                                filePath   = root repo ++ url'
                            dirEntries <- check $ getDirectoryContents filePath
                            let acceptable = filter (`notElem` [".", ".."])
                                dirs       = doesDirectoryExist . (filePath </>)
                            check $ filterM dirs $ acceptable dirEntries

-- exported
getChildNode :: Node -> String -> RepositoryContext Node
getChildNode node name = do let path      = node_path node
                                path'     = path ++ [name]
                                repo      = node_repo node
                            getNode repo path'

--exported
getChildNodes :: Node -> RepositoryContext [Node]
getChildNodes node = getChildNodeNames node >>= mapM (getChildNode node)

--exported
getProperty :: Node -> String -> Maybe Property
getProperty node name = fmap (Property name) $ lookup name props
    where props = map (\(Property n v) -> (n, v)) $ node_props node

--exported
isRootNode :: Node -> Bool
isRootNode n = node_path n == []
