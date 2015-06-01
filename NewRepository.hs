{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module NewRepository where

import Control.Monad                        (Monad, liftM, liftM2)
import Control.Monad.Writer                 (MonadWriter, Writer, runWriter, tell)
import Control.Applicative                  (Applicative)
import Control.Monad.IO.Class               (MonadIO)
import Control.Monad.Trans.Either           (EitherT, runEitherT, left)
import Control.Exception                    (throwIO, catch)
import Data.Functor                         (Functor)
import Data.DateTime                        (DateTime)
import Data.UUID                            (UUID, nil)
import Data.UUID.V4                         (nextRandom)
import System.FilePath                      ((</>))
import System.IO                            (appendFile)
import System.Directory                     (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, removeFile)
import Utils                                (check)
import Debug.Trace

data Transaction = Transaction { ta_uuid :: UUID
                               , ta_ops  :: [Operation]
                               }
    deriving (Show)

data Node     = Node     { path :: String }
    deriving (Show)

data Property = Property { prop_name  :: String
                         , prop_value :: Value
                         }
    deriving (Show)

data Value    = StringValue   { string_value   :: String }
              | IntegerValue  { integer_value  :: Integer }
              | BooleanValue  { boolean_value  :: Bool }
              | DateTimeValue { datetime_value :: DateTime }
    deriving (Show)

data Operation = AddNode Node
               | RemoveNode Node
               | AddProperty Node Property
               | RemoveProperty Node Property
               | ModifyProperty Node Property
    deriving (Show)

newtype TransactionContext a = TC { runTC :: Writer [Operation] a }
    deriving (Monad, Functor, Applicative, MonadWriter [Operation])

data LocalRepository = LocalRepository { localrep_root :: FilePath }
    deriving (Show)

class Monad m => Repository r m | m -> r where
    r_getTransaction :: TransactionContext a  -> EitherT IOError m Transaction
    r_logBegin       :: r -> Transaction      -> EitherT IOError m ()
    r_logEnd         :: r -> Transaction      -> EitherT IOError m ()
    r_addNode        :: r -> Node             -> EitherT IOError m ()
    r_removeNode     :: r -> Node             -> EitherT IOError m ()
    r_addProperty    :: r -> Node -> Property -> EitherT IOError m ()
    r_removeProperty :: r -> Node -> Property -> EitherT IOError m ()
    r_modifyProperty :: r -> Node -> Property -> EitherT IOError m ()

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
    r_addNode        repo node      = check $ do let dir = localrep_root repo ++ path node
                                                 exists <- doesDirectoryExist dir
                                                 if exists
                                                 then throwIO $ userError "node exists already"
                                                 else createDirectoryIfMissing True dir
    r_removeNode     repo node      = check $ do let dir = localrep_root repo ++ path node
                                                 exists <- doesDirectoryExist dir
                                                 if exists
                                                 then removeDirectoryRecursive dir
                                                 else throwIO $ userError "node does not exist"
    r_addProperty    repo node prop = check $ do let dir = localrep_root repo ++ path node
                                                     file = dir ++ '/' : prop_name prop
                                                 exists <- doesDirectoryExist dir
                                                 if exists
                                                 then writeFile file $ show $ prop_value prop
                                                 else throwIO $ userError "node does not exist"
    r_removeProperty repo node prop = check $ do let dir = localrep_root repo ++ path node
                                                     file = dir ++ '/' : prop_name prop
                                                 exists <- doesDirectoryExist dir
                                                 if exists
                                                 then removeFile file
                                                 else throwIO $ userError "node does not exist"
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
    where node     = Node        "/content/docs/index.html"
          property = Property    "sling:resourceType" value
          value    = StringValue "cq:Page"

-- rep_transactionsSince :: a -> UUID -> [Transaction a]
-- rep_transactionsSince (LocalRepository root parent) uuid = [RootTransaction]
-- rep_transactionsSince RemoteRepository              uuid = [RootTransaction]

test_runTransaction = do res <- runTransaction (LocalRepository "/tmp/testrepo") doSomething
                         putStrLn $ show res

test_doSomething = putStrLn $ show $ runWriter $ runTC doSomething
