{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CH.ComeBackGloebb.CBGWebSite.Repo.Types where

import Control.Monad                                (Monad)
import Control.Monad.Writer                         (MonadWriter, Writer)
import Control.Applicative                          (Applicative)
import Data.Functor                                 (Functor)
import Data.DateTime                                (DateTime)
import Data.UUID                                    (UUID)

newtype Node     = Node     { path :: String }
    deriving (Show)

data Property = Property { prop_name  :: String
                         , prop_value :: Value
                         }
    deriving (Show)

data Value    = StringValue   { string_value   :: String }
              | IntegerValue  { integer_value  :: Integer }
              | BooleanValue  { boolean_value  :: Bool }
              | DateTimeValue { datetime_value :: DateTime }
    deriving (Show, Read, Eq)

data Operation = AddNode Node
               | RemoveNode Node
               | AddProperty Node Property
               | RemoveProperty Node Property
               | ModifyProperty Node Property
    deriving (Show)

newtype TransactionContext a = TC { runTC :: Writer [Operation] a }
    deriving (Monad, Functor, Applicative, MonadWriter [Operation])

data Transaction = Transaction { ta_uuid :: UUID
                               , ta_ops  :: [Operation]
                               }
    deriving (Show)
