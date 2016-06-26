{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Event (Event(..), getEventsForMonth) where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           Control.Monad                                     (liftM)
import           Data.Aeson                                        (ToJSON (..),
                                                                    object,
                                                                    (.=))
import qualified Data.ByteString.Lazy.UTF8                         as UL8
import           Data.DateTime                                     (DateTime, fromSqlString,
                                                                    startOfTime,
                                                                    toSqlString)
import           Data.Maybe                                        (fromMaybe)
import qualified Data.Text                                         as T

-- exported
data Event = Event { evRepo        :: Repository
                   , evTitle       :: T.Text
                   , evStartDate   :: DateTime
                   , evEndDate     :: Maybe DateTime
                   , evDescription :: T.Text
                   , evLocation    :: T.Text
                   } deriving (Show)

instance Persistent Event where

  writeItem ev @ Event {..}  = do
    let n = toNode ev
    writeNode n
    writeProperty n "title"       $ UL8.fromString $ T.unpack               evTitle
    writeProperty n "startDate"   $ UL8.fromString $ toSqlString            evStartDate
    writeProperty n "endDate"     $ maybe "" (UL8.fromString . toSqlString) evEndDate
    writeProperty n "description" $ UL8.fromString $ T.unpack               evDescription
    writeProperty n "location"    $ UL8.fromString $ T.unpack               evLocation

  readItem repo path = do
    node            <- getNode repo (urlFromString path)
    let textProperty = liftM (T.pack . UL8.toString) . getProperty node
        dateProperty = liftM (fromSqlString . UL8.toString) . getProperty node
    startDate       <- dateProperty "startDate"
    endDate         <- dateProperty "endDate"
    description     <- textProperty "description"
    location        <- textProperty "location"
    return $ Event repo
                   (T.pack $ node_name node)
                   (fromMaybe startOfTime startDate)
                   endDate
                   description
                   location

  deleteItem = deleteNode . toNode

  toNode event = Node eventName (urlFromString eventName) [] repo
    where eventName = T.unpack $ evTitle event
          repo = evRepo event

instance ToJSON Event where
    toJSON Event {..} = object [ "title"       .=                      evTitle
                               , "startDate"   .=          toSqlString evStartDate
                               , "endDate"     .= maybe "" toSqlString evEndDate
                               , "description" .=                      evDescription
                               , "location"    .=                      evLocation
                               ]

fromNode :: Node -> RepositoryContext Event
fromNode n = readItem (node_repo n) (urlToString $ node_path n)

-- exported
getEventsForMonth :: Repository -> Int -> Int -> RepositoryContext [Event]
getEventsForMonth repo year month = do
  let url = map (pathCompFromString . show) [year, month]
  node <- getNode repo url
  nodes     <- getChildNodesRecursively node
  let nodes' = filter ((== 4) . length . node_path) nodes
  mapM fromNode nodes'
