{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Member (Member(..), memberId, getMemberList) where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           Control.Monad                                     (liftM)
import           Data.Aeson                                        (ToJSON (..),
                                                                    object,
                                                                    (.=))
import qualified Data.ByteString.Lazy.Char8                        as BL8
import           Data.List                                         (sort)
import qualified Data.Text                                         as T

data Member = Member { memRepo      :: Repository
                     , memFirstname :: T.Text
                     , memName      :: T.Text
                     , memAddress   :: T.Text
                     , memLocality  :: T.Text
                     , memStatus    :: T.Text
                     , memPhone     :: T.Text
                     , memMobile    :: T.Text
                     , memEmail     :: T.Text
                     } deriving (Show, Eq)

instance Ord Member where
  compare a b = compare (key a) (key b)
    where
      key m = show (memName m) ++ show (memFirstname m)

instance ToJSON Member where
    toJSON Member {..} = object ["firstname" .= memFirstname
                                ,"name"      .= memName
                                ,"address"   .= memAddress
                                ,"locality"  .= memLocality
                                ,"status"    .= memStatus
                                ,"phone"     .= memPhone
                                ,"mobile"    .= memMobile
                                ,"email"     .= memEmail
                                ]

instance Persistent Member where

  readItem repo path = do
    node        <- getNode repo (urlFromString path)
    let property = liftM (T.pack . show) . getProperty node
    firstname   <- property "firstname"
    name        <- property "name"
    address     <- property "address"
    locality    <- property "locality"
    status      <- property "status"
    phone       <- property "phone"
    mobile      <- property "mobile"
    email       <- property "email"
    return $ Member repo
                    firstname
                    name
                    address
                    locality
                    status
                    phone
                    mobile
                    email

  writeItem member @ Member {..} = do
    let n = toNode member
    writeNode n
    writeProperty n "firstname" $ BL8.pack $ T.unpack memFirstname
    writeProperty n "name"      $ BL8.pack $ T.unpack memName
    writeProperty n "address"   $ BL8.pack $ T.unpack memAddress
    writeProperty n "locality"  $ BL8.pack $ T.unpack memLocality
    writeProperty n "status"    $ BL8.pack $ T.unpack memStatus
    writeProperty n "phone"     $ BL8.pack $ T.unpack memPhone
    writeProperty n "mobile"    $ BL8.pack $ T.unpack memMobile
    writeProperty n "email"     $ BL8.pack $ T.unpack memEmail

  deleteItem = deleteNode . toNode

  toNode member = Node memberName (urlFromString memberName) [] repo
    where memberName = T.unpack $ memberId member
          repo = memRepo member

-- exported
memberId :: Member -> T.Text
memberId member = T.concat [memName member, T.pack " ", memFirstname member]

-- exported
getMemberList :: Repository -> RepositoryContext [Member]
getMemberList repo = do
  node <- getNode repo (urlFromString "/")
  children <- getChildNodeNames node
  liftM sort $ mapM (readItem repo) children
