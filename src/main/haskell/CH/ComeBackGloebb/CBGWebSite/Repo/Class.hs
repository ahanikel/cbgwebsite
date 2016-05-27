{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module CH.ComeBackGloebb.CBGWebSite.Repo.Class where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Types
import           Control.Monad                           (Monad)
import           Control.Monad.Trans.Either              (EitherT)

class Monad m => Repository r m | m -> r where
    r_getTransaction :: TransactionContext a  -> EitherT IOError m Transaction
    r_logBegin       :: r -> Transaction      -> EitherT IOError m ()
    r_logEnd         :: r -> Transaction      -> EitherT IOError m ()
    r_getNode        :: r -> FilePath         -> EitherT IOError m Node
    r_addNode        :: r -> Node             -> EitherT IOError m ()
    r_removeNode     :: r -> Node             -> EitherT IOError m ()
    r_addProperty    :: r -> Node -> Property -> EitherT IOError m ()
    r_removeProperty :: r -> Node -> Property -> EitherT IOError m ()
    r_modifyProperty :: r -> Node -> Property -> EitherT IOError m ()
