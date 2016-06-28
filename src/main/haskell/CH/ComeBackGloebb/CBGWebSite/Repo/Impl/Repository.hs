module CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
    ( Repository (..)
    , openRepository
    , RepositoryContext
    , Node (..)
    , getNode
    , writeNode
    , deleteNode
    , overwriteNode
    , getParentNode
    , getChildNodeNames
    , getChildNode
    , getChildNodes
    , getChildNodesRecursively
    , getSiblingNames
    , getSiblings
    , Property (..)
    , getProperty
    , getPropertyWithDefault
    , hasProperty
    , getPropertyPath
    , writeProperty
    , deleteProperty
    , Value (..)
    , URL
    , urlFromString
    , urlFromStrings
    , urlToString
    , urlFromFilePath
    , urlToFilePath
    , PathComponent
    , pathCompFromFilePath
    , pathCompFromString
    , isRootNode
    , Persistent(..)
    )
where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Utils
import           Control.Monad                                (filterM, liftM)
import           Control.Monad.Trans.Either                   (EitherT, left)
import           Control.Monad.Trans.Either                   (runEitherT)
import qualified Data.ByteString.Lazy                         as BL
import           Data.List                                    (intercalate,
                                                               isSuffixOf)
import           System.Directory                             (createDirectoryIfMissing,
                                                               doesDirectoryExist,
                                                               doesFileExist,
                                                               getDirectoryContents,
                                                               removeFile)
import           System.Exit                                  (ExitCode (..))
import           System.FilePath                              (normalise,
                                                               splitDirectories,
                                                               (</>))
import           System.Process                               (rawSystem)

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
pathCompFromFilePath :: FilePath -> PathComponent
pathCompFromFilePath = init . init -- remove ".n"/".p" suffix

-- exported
pathCompFromString :: String -> PathComponent
pathCompFromString s = if s == ".." then "" else filter (/= '/') s

-- exported
type URL = [PathComponent]

-- exported
urlFromFilePath :: FilePath -> URL
urlFromFilePath ""      = []
urlFromFilePath ('/':s) = urlFromFilePath s
urlFromFilePath s       = map pathCompFromFilePath $ splitDirectories $ normalise s

-- exported
urlFromString :: String -> URL
urlFromString ""      = []
urlFromString ('/':s) = urlFromString s
urlFromString s       = map pathCompFromString $ splitDirectories $ normalise s

-- exported
urlFromStrings :: [String] -> URL
urlFromStrings []     = []
urlFromStrings ss     = filter (/= "") $ map pathCompFromString ss

-- exported
urlToFilePath :: URL -> FilePath
urlToFilePath [] = ""
urlToFilePath url = intercalate ".n/" url ++ ".n"

-- exported
urlToString :: URL -> String
urlToString =  intercalate "/"

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
data Property = Property { prop_name  :: String
                         , prop_value :: Value
                         }
    deriving (Read, Show, Eq)

-- exported
data Node = Node { node_name  :: String
                 , node_path  :: URL
                 , node_props :: [String]
                 , node_repo  :: Repository
                 }
    deriving (Read, Show, Eq)

-- exported
type RepositoryContext a = EitherT IOError IO a

readPropertyNames :: FilePath -> IO [String]
readPropertyNames path = readFiles           >>=
                         filterM filterFiles >>=
                         mapM (\(pname, _) -> return $ pathCompFromFilePath pname)
    where
          -- return a list of pairs (fileName, completePath), e.g. ("text.md", "content/welcome/text.md")
          readFiles :: IO [(FilePath, FilePath)]
          readFiles                  = getDirectoryContents path >>= return . map (\n -> (pathCompFromFilePath n, path </> n))
          filterFiles (_, path')     = do exists <- doesFileExist path'
                                          let isProp = isSuffixOf ".p" path'
                                          return $ exists && isProp

-- exported
getProperty :: Node -> String -> RepositoryContext BL.ByteString
getProperty node pname = check $ BL.readFile $ getPropertyPath node pname

-- exported
getPropertyWithDefault :: Node -> String -> BL.ByteString -> RepositoryContext BL.ByteString
getPropertyWithDefault node pname def = do eProp <- check $ runEitherT $ getProperty node pname
                                           return $ either (const def) id eProp

-- exported
hasProperty :: Node -> String -> RepositoryContext Bool
hasProperty node pname = check $ doesFileExist $ getPropertyPath node pname

-- exported
getPropertyPath :: Node -> String -> FilePath
getPropertyPath node pname = root repo </> path </> fileName
  where repo = node_repo node
        path = urlToFilePath $ node_path node
        fileName = pathCompFromString pname ++ ".p"

-- exported
writeProperty :: Node -> String -> BL.ByteString -> RepositoryContext ()
writeProperty node pname bytes = check $ BL.writeFile (getPropertyPath node pname) bytes

-- exported
deleteProperty :: Node -> String -> RepositoryContext ()
deleteProperty node pname = check $ removeFile $ getPropertyPath node pname

-- exported
getNode :: Repository -> URL -> RepositoryContext Node
getNode repo url = do let name       = case url of [] -> "/"
                                                   _  -> last url
                          filePath   = root repo </> urlToFilePath url
                      props <- check (readPropertyNames filePath)
                      return $ Node name url props repo

--exported
writeNode :: Node -> RepositoryContext ()
writeNode node = check $ do let path     = node_path node
                                path'    = urlToFilePath path
                                repo     = node_repo node
                                filePath = root repo </> path'
                            createDirectoryIfMissing True filePath

--exported
deleteNode :: Node -> RepositoryContext ()
deleteNode node = check $ do let path     = node_path node
                                 path'    = urlToFilePath path
                                 repo     = node_repo node
                                 filePath = root repo </> path'
                             exitCode <- rawSystem "/bin/rm" ["-rf", filePath]
                             case exitCode of ExitFailure n -> ioError $ userError $ "deleteNode failed with exit code " ++ show n
                                              ExitSuccess   -> return ()

--exported
overwriteNode :: Node -> RepositoryContext ()
overwriteNode node = do deleteNode node
                        writeNode node

-- exported
getParentNode :: Node -> RepositoryContext (Maybe Node)
getParentNode node | node_name node == "/" = return Nothing
getParentNode node | otherwise = liftM Just $ getNode (node_repo node) (init $ node_path node)

-- exported
getChildNodeNames :: Node -> RepositoryContext [String]
getChildNodeNames node = check $ do let path        = node_path node
                                        path'       = urlToFilePath path
                                        repo        = node_repo node
                                        filePath    = root repo </> path'
                                    dirEntries <- getDirectoryContents filePath
                                    let acceptable  = filter (`notElem` [".", ".."]) dirEntries
                                        isNodeDir d = do let dir = filePath </> d
                                                         exists <- doesDirectoryExist dir
                                                         let isNode = isSuffixOf ".n" dir
                                                         return (exists && isNode)
                                    nodeDirs <- filterM isNodeDir acceptable
                                    return $ map pathCompFromFilePath nodeDirs

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
getChildNodesRecursively :: Node -> RepositoryContext [Node]
getChildNodesRecursively node = do
  nodes <- getChildNodes node
  nodes' <- mapM getChildNodesRecursively nodes
  return $ nodes ++ concat nodes'

--exported
getSiblingNames :: Node -> RepositoryContext [String]
getSiblingNames node = do mparent <- getParentNode node
                          case mparent of
                            Nothing -> return []
                            Just parent -> do
                              let path = urlToFilePath $ node_path $ parent
                                  repo = node_repo parent
                                  filePath = root repo </> path
                              dirEntries <- check $ getDirectoryContents filePath
                              let acceptable = (`notElem` [".", "..", node_name node])
                                  isNodeDir d = do let dir = filePath </> d
                                                   exists <- doesDirectoryExist dir
                                                   let isNode = ".n" `isSuffixOf` dir
                                                   return (exists && isNode)
                              nodeDirs <- check $ filterM isNodeDir $ filter acceptable dirEntries
                              return $ map pathCompFromFilePath nodeDirs
--exported
getSiblings :: Node -> RepositoryContext [Node]
getSiblings node = do
  mparent <- getParentNode node
  case mparent of
    Nothing -> return []
    Just parent -> do
      siblingNames <- getSiblingNames node
      mapM (getChildNode parent) siblingNames

--exported
isRootNode :: Node -> Bool
isRootNode n = node_path n == []

class Persistent a where
  writeItem  :: a -> RepositoryContext ()
  readItem   :: Repository -> String -> RepositoryContext a
  deleteItem :: a -> RepositoryContext ()
  toNode     :: a -> Node
