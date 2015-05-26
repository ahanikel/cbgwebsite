module NewRepository where

class Repository where
    getTransactionsSince :: UUID -> [Transaction]

data LocalRepository = Repository { rep_root :: FilePath
                                  , rep_parent :: Maybe Repository
                                  }

data Transaction = Transaction { ta_uuid :: UUID
                               , ta_ops  :: [Operation]
                               }
                   RootTransaction

data Operation = AddNode Node
               | RemoveNode Node
               | AddProperty Node Property
               | RemoveProperty Node Property
               | ModifyProperty Node Property
