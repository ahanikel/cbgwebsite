{-# LANGUAGE BangPatterns #-}

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
    , Property (..)
    , getProperty
    , getPropertyPath
    , readBlobProperty
    , writeBlobProperty
    , Value (..)
    , URL
    , urlFromString
    , urlToString
    , urlFromFilePath
    , urlToFilePath
    , PathComponent
    , pathCompFromFilePath
    , pathCompFromString
    , isRootNode
    , Persistent (..)
    )
where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Utils
import           Control.Monad                                (filterM, liftM)
import           Control.Monad.Trans.Either                   (EitherT, left,
                                                               runEitherT)
import qualified Data.ByteString.Lazy                         as BL
import           Data.List                                    (intercalate,
                                                               isSuffixOf)
import           System.Directory                             (createDirectoryIfMissing,
                                                               doesDirectoryExist,
                                                               doesFileExist,
                                                               getDirectoryContents)
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
pathCompFromString = id

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
                 , node_props :: ![Property]
                 , node_repo  :: Repository
                 }
    deriving (Read, Show, Eq)

-- exported
type RepositoryContext a = EitherT IOError IO a

readProperties :: FilePath -> IO [Property]
readProperties path = readFiles           >>=
                      filterM filterFiles >>=
                      mapM readProperty
    where
          -- return a list of pairs (fileName, completePath), e.g. ("text.md", "content/welcome/text.md")
          readFiles :: IO [(FilePath, FilePath)]
          readFiles                  = getDirectoryContents path >>= return . map (\n -> (pathCompFromFilePath n, path </> n))
          filterFiles (_, path')     = do exists <- doesFileExist path'
                                          let isProp = isSuffixOf ".p" path'
                                          return $ exists && isProp
          readProperty (name, path') = readFile path' >>= return . Property name . StringValue

writeProperties :: [Property] -> FilePath -> IO ()
writeProperties props path = mapM_ writeProperty props
    where writeProperty prop = do let name     = prop_name prop ++ ".p"
                                      value    = show $ prop_value prop
                                      filePath = path </> name
                                  writeFile filePath value

-- exported
getProperty :: Node -> String -> Maybe Property
getProperty node pname = case props of
  (prop:_) -> Just prop
  _ -> Nothing

  where props = filter (\p -> prop_name p == pname) $ node_props node

-- exported
getPropertyPath :: Node -> String -> FilePath
getPropertyPath node pname = root repo </> path </> fileName
  where repo = node_repo node
        path = urlToFilePath $ node_path node
        fileName = pname ++ ".p"

-- exported
writeBlobProperty :: Node -> String -> BL.ByteString -> RepositoryContext ()
writeBlobProperty node name bytes = do
  let path = node_path node
      path' = urlToFilePath path
      repo  = node_repo node
      fileName = name ++ ".blob"
      filePath = root repo </> path' </> fileName
  check $ BL.writeFile filePath bytes

-- exported
readBlobProperty :: Node -> String -> RepositoryContext BL.ByteString
readBlobProperty node name = do
  let path = node_path node
      path' = urlToFilePath path
      repo = node_repo node
      fileName = name ++ ".blob"
      filePath = root repo </> path' </> fileName
  check $ BL.readFile filePath

-- exported
getNode :: Repository -> URL -> RepositoryContext Node
getNode repo url = do let name       = case url of [] -> "/"
                                                   _  -> last url
                          filePath   = root repo </> urlToFilePath url
                      props <- check (readProperties filePath)
                      return $ Node name url props repo

--exported
writeNode :: Node -> RepositoryContext ()
writeNode node = check $ do let path     = node_path node
                                path'    = urlToFilePath path
                                repo     = node_repo node
                                filePath = root repo </> path'
                                props    = node_props node
                            createDirectoryIfMissing True filePath
                            writeProperties props filePath

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
getParentNode :: Node -> RepositoryContext Node
getParentNode node | node_name node == "/" = left $ userError "Root has no parent"
getParentNode node | otherwise = do let path     = init $ node_path node
                                    getNode (node_repo node) path

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
isRootNode :: Node -> Bool
isRootNode n = node_path n == []

--exported
--deprecated: this abstraction is flawed
class Persistent a where
    toNode       :: Repository -> String -> a -> Node
    fromNode     :: Node -> a
    writeItem    :: Repository -> String -> a -> IO (Either IOError ())
    writeItem repo name a = runEitherT $ writeNode $ toNode repo name a
    readItem     :: Repository -> String -> IO (Either IOError a)
    readItem repo name = runEitherT $ liftM fromNode $ getNode repo $ urlFromString name
