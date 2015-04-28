module Repository
    ( Repository
    , openRepository
    , Node
    , getNode
    , getParentNode
    , getChildNodeNames
    , getChildNode
    , Property
    , Value
    , URL
    , urlFromString
    , PathComponent
    , pathCompFromString
    )
where

import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import Control.Monad (filterM)
import System.FilePath (normalise, splitDirectories, (</>))
import Control.Monad.Error (ErrorT(..))
import Control.Exception (catch, SomeException, IOException, try)

-- exported
data Repository = Repository { root :: FilePath
                             }
    deriving (Read, Show, Eq)

-- exported
openRepository :: FilePath -> Repository
openRepository = Repository

-- exported
type URL = String

-- exported
urlFromString :: String -> URL
urlFromString = id

-- exported
type PathComponent = String

-- exported
pathCompFromString :: String -> PathComponent 
pathCompFromString = id

-- exported
data Value = StringValue String
           | NumberValue Integer
           | BooleanValue Bool
    deriving (Read, Show, Eq)

-- exported
data Property = Property { prop_name :: String
                         , prop_value :: Value
                         }
    deriving (Read, Show, Eq)

-- exported
data Node = Node { node_name        :: String
                 , node_path        :: [PathComponent]
                 , node_props       :: [Property]
                 , node_repo        :: Repository
                 }
    deriving (Read, Show, Eq)

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

-- from https://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html
check :: IO a -> ErrorT IOException IO a
check io = ErrorT (try io)

-- exported
getNode :: Repository -> URL -> ErrorT IOException IO Node
getNode repo url = do let normalised = normalise url
                          path       = splitDirectories normalised
                          name       = last path
                          url'       = if head normalised == '/'
                                       then normalised
                                       else '/' : normalised
                          filePath   = root repo ++ url'
                      props <- check (readProperties filePath)
                      return $ Node name path props repo

-- exported
getParentNode :: Node -> ErrorT IOException IO Node
getParentNode node = do let path     = init $ node_path node
                            name     = last path
                            url'     = '/' : foldr (</>) "" path
                            repo     = node_repo node
                            filePath = root repo ++ url'
                        props <- check $ readProperties filePath
                        return $ Node name path props repo

-- exported
getChildNodeNames :: Node -> ErrorT IOException IO [String]
getChildNodeNames node = do let path       = node_path node
                                url'       = '/' : foldr (</>) "" path
                                repo       = node_repo node
                                filePath   = root repo ++ url'
                            dirEntries <- check $ getDirectoryContents filePath
                            let acceptable = filter (`notElem` [".", ".."])
                                dirs       = doesDirectoryExist . (filePath </>)
                            check $ filterM dirs $ acceptable dirEntries

-- exported
getChildNode :: Node -> String -> ErrorT IOException IO Node
getChildNode node name = do let path      = node_path node
                                url       = '/' : foldr (</>) "" path
                                url'      = url </> name
                                repo      = node_repo node
                            getNode repo url'
