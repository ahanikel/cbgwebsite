{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module CH.ComeBackGloebb.CBGWebSite.Repo.Impl.FileStorage
    --( test_runTransaction
    --, test_doSomething
    --)
where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Class      (Repository (..))
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Utils (check)
import           CH.ComeBackGloebb.CBGWebSite.Repo.Types

import           Control.Exception                            (throwIO)
import           Control.Monad                                (filterM, when)
import           Control.Monad.Trans.Either                   (left, runEitherT)
import           Control.Monad.Writer                         (Writer,
                                                               runWriter, tell)
import           Data.Hash.MD5                                (Str (..), md5s)
import           Data.UUID                                    (fromString, nil)
import           Data.UUID.V4                                 (nextRandom)
import           System.Directory                             (createDirectoryIfMissing,
                                                               doesDirectoryExist,
                                                               doesFileExist,
                                                               getDirectoryContents,
                                                               removeFile)
import           System.FilePath                              ((</>))


------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------

data LocalRepository = LocalRepository { localrep_root :: FilePath }
    deriving (Show)


------------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------------

uuidPath :: Node -> FilePath
uuidPath node = '/' : a : b : '/' : c : d : '/' : e : f : '/' : u
    where u@(a : b : c : d : e : f : _)    = show $ node_uuid node

propValuePath :: Value -> FilePath
propValuePath value = '/' : a : b : '/' : c : d : '/' : e : f : '/' : hash
    where hash@(a : b : c : d : e : f : _) = md5s $ Str $ show value

propPathPath :: FilePath -> FilePath
propPathPath path = '/' : a : b : '/' : c : d : '/' : e : f : '/' : hash
    where hash@(a : b : c : d : e : f : _) = md5s $ Str $ show path


------------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------------

data FakeRepository = FakeRepository

instance Repository FakeRepository TransactionContext where
    r_getTransaction _              = left $ userError "r_getTransaction not implemented"
    r_logBegin       _    _         = left $ userError "r_logBegin not implemented"
    r_logEnd         _    _         = left $ userError "r_logEnd not implemented"
    r_getNode        _    _         = left $ userError "r_getNode not implemented"
    r_addNode        _    node      = tell [AddNode        node]
    r_removeNode     _    node      = tell [RemoveNode     node]
    r_addProperty    _    node prop = tell [AddProperty    node prop]
    r_removeProperty _    node prop = tell [RemoveProperty node prop]
    r_modifyProperty _    node prop = tell [ModifyProperty node prop]

instance Repository LocalRepository IO where
    r_getTransaction tc             = check $ do let ops = snd $ runWriter $ runTC tc
                                                 uuid <- nextRandom
                                                 return $ Transaction uuid ops
    r_logBegin repo trans           = check $ do createDirectoryIfMissing True $ localrep_root repo
                                                 let localrep_logbegin = localrep_root repo </> "begin.log"
                                                 appendFile localrep_logbegin  $ show trans ++ "\n"
    r_logEnd   repo trans           = check $ do let localrep_logend   = localrep_root repo </> "end.log"
                                                 appendFile localrep_logend    $ show trans ++ "\n"
    r_getNode        repo path      = check $ do let pathdir     = localrep_root repo ++ "/path" ++ propPathPath path
                                                 entries        <- getDirectoryContents pathdir
                                                                   >>= mapM (\e -> return (e, (pathdir </> e)))
                                                                   >>= filterM (\(_, f) -> doesFileExist f)
                                                                   >>= filterM (\(_, f) -> readFile f >>= (\p -> return $ p == path))
                                                 case entries of
                                                   [(entry,_)] -> do
                                                     let (Just uuid) = fromString entry
                                                     return $ Node uuid path
                                                   _ -> fail "Node not found"
    r_addNode        repo node      = check $ do let uuiddir = localrep_root repo ++ "/uuid" ++ uuidPath node
                                                 let pathdir = localrep_root repo ++ "/path" ++ (propPathPath $ node_path node)
                                                 existsu <- doesDirectoryExist uuiddir
                                                 when existsu $ throwIO $ userError "node exists already"
                                                 existsp <- doesDirectoryExist pathdir
                                                 when existsp $ throwIO $ userError "path exists already"
                                                 createDirectoryIfMissing True uuiddir
                                                 writeFile (uuiddir </> "uuid") (show $ node_uuid node)
                                                 writeFile (uuiddir </> "path") (node_path node)
                                                 createDirectoryIfMissing True pathdir
                                                 writeFile (pathdir </> (show $ node_uuid node)) (node_path node)
    r_removeNode     _    _         = undefined
--     r_removeNode     repo node      = check $ do n <- r_getNode repo (node_path node)
--                                                  children <- findChildren n
--                                                  mapM_ (r_removeNode repo) children
--                                                  let uuiddir = localrep_root repo ++ "/uuid" ++ uuidPath n
--                                                  let pathdir = localrep_root repo ++ "/path" ++ (propPathPath $ node_path n)
--                                                  remove pathdir
--                                                  remove uuiddir
    r_addProperty    repo node prop = check $ do let uuiddir  = localrep_root repo ++ "/uuid" ++ uuidPath node
                                                     pathdir  = localrep_root repo </> prop_name prop ++ (propPathPath $ node_path node)
                                                     uuidFile = uuiddir </> prop_name prop
                                                     pathFile = pathdir </> (show $ node_uuid node)
                                                 existsu  <- doesDirectoryExist uuiddir
                                                 when (not existsu) (throwIO $ userError "node does not exist")
                                                 writeFile uuidFile $ show $ prop_value prop
                                                 writeFile pathFile $ show $ prop_value prop
    r_removeProperty repo node prop = check $ do let uuiddir  = localrep_root repo ++ "/uuid" ++ uuidPath node
                                                     pathdir  = localrep_root repo </> prop_name prop ++ (propPathPath $ node_path node)
                                                     uuidFile = uuiddir </> prop_name prop
                                                     pathFile = pathdir </> (show $ node_uuid node)
                                                 existsu  <- doesDirectoryExist uuiddir
                                                 when (not existsu) (throwIO $ userError "node does not exist")
                                                 existsuf <- doesFileExist uuidFile
                                                 when (not existsuf) (throwIO $ userError "property does not exist")
                                                 existsp  <- doesDirectoryExist pathdir
                                                 existspf <- doesFileExist pathFile
                                                 if   existsp && existspf
                                                 then removeFile pathFile
                                                 else return ()
    r_modifyProperty                = r_addProperty -- for now...

instance Repository FakeRepository (Writer [String]) where
    r_getTransaction                = return . Transaction nil . snd . runWriter . runTC
    r_logBegin       _    trans     = tell ["r_logBegin "       ++ show trans]
    r_logEnd         _    trans     = tell ["r_logEnd "         ++ show trans]
    r_getNode        _    path      = tell ["r_getNode "        ++ show path] >> (return $ Node nil path)
    r_addNode        _    node      = tell ["r_addNode "        ++ show node]
    r_removeNode     _    node      = tell ["r_removeNode "     ++ show node]
    r_addProperty    _    node prop = tell ["r_addProperty "    ++ show node ++ " " ++ show prop]
    r_removeProperty _    node prop = tell ["r_removeProperty " ++ show node ++ " " ++ show prop]
    r_modifyProperty _    node prop = tell ["r_modifyProperty " ++ show node ++ " " ++ show prop]

runTransaction :: Repository r m => r -> TransactionContext a -> m (Either IOError Transaction)
runTransaction repo tc = runEitherT $ do trans <- r_getTransaction tc
                                         r_logBegin repo trans
                                         mapM_ exec_op $ ta_ops trans
                                         r_logEnd   repo trans
                                         return trans
    where exec_op op = case op of AddNode        node      -> r_addNode        repo node
                                  RemoveNode     node      -> r_removeNode     repo node
                                  AddProperty    node prop -> r_addProperty    repo node prop
                                  RemoveProperty node prop -> r_removeProperty repo node prop
                                  ModifyProperty node prop -> r_modifyProperty repo node prop

doSomething :: TransactionContext ()
doSomething = do _ <- runEitherT $ do --r_removeNode  FakeRepository node
                                      r_addNode     FakeRepository node
                                      r_addProperty FakeRepository node property
                 return ()
    where node     = Node        nil "/content/docs/index.html"
          property = Property    "sling:resourceType" value
          value    = StringValue "cq:Page"

-- rep_transactionsSince :: a -> UUID -> [Transaction a]
-- rep_transactionsSince (LocalRepository root parent) uuid = [RootTransaction]
-- rep_transactionsSince RemoteRepository              uuid = [RootTransaction]

test_runTransaction :: IO ()
test_runTransaction = do res <- runTransaction (LocalRepository "/tmp/testrepo") doSomething
                         putStrLn $ show res

test_doSomething :: IO ()
test_doSomething = putStrLn $ show $ runWriter $ runTC doSomething
