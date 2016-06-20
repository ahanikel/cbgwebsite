{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CH.ComeBackGloebb.CBGWebSite.Repo.Types where

import           Control.Monad.Writer (MonadWriter, Writer)
import           Data.DateTime        (DateTime)
import           Data.UUID            (UUID)

newtype PathComponent = PathComponent String
    deriving (Show)

type URL      = [PathComponent]

data Node     = Node { node_uuid :: UUID
                     , node_path :: FilePath
                     }
    deriving (Show)

data Property = Property { prop_name  :: String
                         , prop_value :: Value
                         }
    deriving (Show)

data Value    = StringValue   { string_value :: String }
              | IntegerValue  { integer_value :: Integer }
              | BooleanValue  { boolean_value :: Bool }
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
