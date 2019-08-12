{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module CH.ComeBackGloebb.CBGWebSite.Repo.Class where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Types
import           Control.Monad.Except                    (ExceptT)

class Monad m => Repository r m | m -> r where
    r_getTransaction :: TransactionContext a  -> ExceptT IOError m Transaction
    r_logBegin       :: r -> Transaction      -> ExceptT IOError m ()
    r_logEnd         :: r -> Transaction      -> ExceptT IOError m ()
    r_getNode        :: r -> FilePath         -> ExceptT IOError m Node
    r_addNode        :: r -> Node             -> ExceptT IOError m ()
    r_removeNode     :: r -> Node             -> ExceptT IOError m ()
    r_addProperty    :: r -> Node -> Property -> ExceptT IOError m ()
    r_removeProperty :: r -> Node -> Property -> ExceptT IOError m ()
    r_modifyProperty :: r -> Node -> Property -> ExceptT IOError m ()
