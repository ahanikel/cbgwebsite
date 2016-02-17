{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module CH.ComeBackGloebb.CBGWebSite.Repo.Impl.FileStorage
    (
    )
where

import CH.ComeBackGloebb.CBGWebSite.Repo.Class      (Repository(..))
import CH.ComeBackGloebb.CBGWebSite.Repo.Types
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Utils (check)

import Control.Monad                                (when)
import Control.Monad.Writer                         (Writer, runWriter, tell)
import Control.Monad.Trans.Either                   (runEitherT, left)
import Control.Exception                            (throwIO)
import Data.Hash.MD5                                (md5s, Str(..))
import Data.UUID                                    (nil)
import Data.UUID.V4                                 (nextRandom)
import System.FilePath                              ((</>))
import System.IO                                    (appendFile)
import System.Directory                             (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)


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

instance Repository FakeRepository TransactionContext where
    r_getTransaction tc             = left $ userError "r_getTransaction not implemented"
    r_logBegin       repo trans     = left $ userError "r_logBegin not implemented"
    r_logEnd         repo trans     = left $ userError "r_logEnd not implemented"
    r_addNode        repo node      = tell [AddNode        node]
    r_removeNode     repo node      = tell [RemoveNode     node]
    r_addProperty    repo node prop = tell [AddProperty    node prop]
    r_removeProperty repo node prop = tell [RemoveProperty node prop]
    r_modifyProperty repo node prop = tell [ModifyProperty node prop]

instance Repository LocalRepository IO where
    r_getTransaction tc             = check $ do let ops = snd $ runWriter $ runTC tc
                                                 uuid <- nextRandom
                                                 return $ Transaction uuid ops
    r_logBegin repo trans           = check $ do createDirectoryIfMissing True $ localrep_root repo
                                                 let localrep_logbegin = localrep_root repo </> "begin.log"
                                                 appendFile localrep_logbegin  $ show trans ++ "\n"
    r_logEnd   repo trans           = check $ do let localrep_logend   = localrep_root repo </> "end.log"
                                                 appendFile localrep_logend    $ show trans ++ "\n"
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
    r_removeNode     repo node      = undefined
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

data FakeRepository = FakeRepository

instance Repository FakeRepository (Writer [String]) where
    r_getTransaction                = return . Transaction nil . snd . runWriter . runTC
    r_logBegin repo trans           = tell ["r_logBegin "       ++ show trans]
    r_logEnd repo trans             = tell ["r_logEnd "         ++ show trans]
    r_addNode        repo node      = tell ["r_addNode "        ++ show node]
    r_removeNode     repo node      = tell ["r_removeNode "     ++ show node]
    r_addProperty    repo node prop = tell ["r_addProperty "    ++ show node ++ " " ++ show prop]
    r_removeProperty repo node prop = tell ["r_removeProperty " ++ show node ++ " " ++ show prop]
    r_modifyProperty repo node prop = tell ["r_modifyProperty " ++ show node ++ " " ++ show prop]

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
doSomething = do _ <- runEitherT $ do r_removeNode  FakeRepository node
                                      r_addNode     FakeRepository node
                                      r_addProperty FakeRepository node property
                 return ()
    where node     = Node        nil "/content/docs/index.html"
          property = Property    "sling:resourceType" value
          value    = StringValue "cq:Page"

-- rep_transactionsSince :: a -> UUID -> [Transaction a]
-- rep_transactionsSince (LocalRepository root parent) uuid = [RootTransaction]
-- rep_transactionsSince RemoteRepository              uuid = [RootTransaction]

test_runTransaction = do res <- runTransaction (LocalRepository "/tmp/testrepo") doSomething
                         putStrLn $ show res

test_doSomething = putStrLn $ show $ runWriter $ runTC doSomething
