{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE BangPatterns      #-}

module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Event (Event(..), newEvent, getEventsForMonth) where

import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           Control.Monad                                     (liftM)
import           Control.Monad.Trans.Either                        (runEitherT)
import           Data.Aeson                                        (ToJSON (..),
                                                                    object,
                                                                    (.=),
                                                                    FromJSON (..),
                                                                    (.:),
                                                                    Value (..))
import qualified Data.ByteString.UTF8                              as U8
import           Data.DateTime                                     (DateTime, fromSqlString,
                                                                    startOfTime,
                                                                    toSqlString,
                                                                    toGregorian',
                                                                    getCurrentTime)
import           Data.Maybe                                        (fromMaybe)
import qualified Data.Text                                         as T
import           Data.UUID                                         (UUID)
import           Data.UUID.V4                                      (nextRandom)
import Debug.Trace

-- exported
data Event = Event { evUUID        :: UUID
                   , evRepo        :: Repository
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
    writeProperty n "uuid"        $ U8.fromString $ show                   evUUID
    writeProperty n "title"       $ U8.fromString $ T.unpack               evTitle
    writeProperty n "startDate"   $ U8.fromString $ toSqlString            evStartDate
    writeProperty n "endDate"     $ maybe "" (U8.fromString . toSqlString) evEndDate
    writeProperty n "description" $ U8.fromString $ T.unpack               evDescription
    writeProperty n "location"    $ U8.fromString $ T.unpack               evLocation
    writeNode indexNode

    where
      indexNode = Node uuid (urlFromStrings indexPath) [] evRepo
      uuid = show evUUID
      indexPath = ["byStartDate", show year, show month, show day, uuid]
      (year, month, day) = toGregorian' evStartDate

  readItem evRepo path = do
    node                           <- getNode evRepo (urlFromString path)
    let dateProperty                = liftM (fromSqlString . U8.toString) . getProperty node
        datePropertyWithDefault def = liftM (fromSqlString . U8.toString) . getPropertyWithDefault node def
        textPropertyWithDefault def = liftM (T.pack . U8.toString) . getPropertyWithDefault node def
        evUUID                      = read $ node_name node :: UUID
    evTitle       <- textPropertyWithDefault "title" ""
    evStartDate   <- fromMaybe startOfTime <$> dateProperty "startDate"
    evEndDate     <- datePropertyWithDefault "endDate" (U8.fromString $ show evStartDate)
    evDescription <- textPropertyWithDefault "description" (U8.fromString "")
    evLocation    <- textPropertyWithDefault "location" (U8.fromString "")
    return $ Event {..}

  deleteItem ev @ Event {..} = do
    deleteNode indexNode
    deleteNode $ toNode ev
    where
      indexNode = Node uuid (urlFromStrings indexPath) [] evRepo
      uuid = show evUUID
      indexPath = ["byStartDate", show year, show month, show day, uuid]
      (year, month, day) = toGregorian' evStartDate

  toNode event = Node eventName (urlFromString eventName) [] repo
    where eventName = show $ evUUID event
          repo = evRepo event
  
instance ToJSON Event where
    toJSON Event {..} = object [ "uuid"        .= show                 evUUID
                               , "title"       .=                      evTitle
                               , "startDate"   .=          toSqlString evStartDate
                               , "endDate"     .= maybe "" toSqlString evEndDate
                               , "description" .=                      evDescription
                               , "location"    .=                      evLocation
                               ]

instance FromJSON Event where
  parseJSON (Object v) =
    Event
      <$> (read <$> T.unpack <$> v .: "uuid")
      <*> (pure $ Repository "")
      <*> v .: "title"
      <*> v .: "startDate"
      <*> v .: "endDate"
      <*> v .: "description"
      <*> v .: "location"
      
fromNode :: Node -> RepositoryContext Event
fromNode n = readItem (node_repo n) (urlToString $ node_path n)

-- exported
getEventsForMonth :: Repository -> Int -> Int -> RepositoryContext [Event]
getEventsForMonth repo year month = do
  let url = map pathCompFromString ["byStartDate", show year, show month]
  monthNode <- getNode repo url
  indexNodes <- getChildNodesRecursively monthNode
  let indexNodes' = filter ((== 5) . length . node_path) indexNodes
  let uuids = map node_name indexNodes'
  eventNodes <- mapM (getNode repo . return) uuids
  mapM fromNode eventNodes

-- exported
newEvent :: Repository -> T.Text -> DateTime -> Maybe DateTime -> T.Text -> T.Text -> IO Event
newEvent evRepo evTitle evStartDate evEndDate evDescription evLocation = do
  evUUID <- nextRandom
  return $ Event {..}

--
-- Tests
--

testWriteEvent = do
  let repo = Repository "data/calendar"
      startDate = fromMaybe startOfTime $ fromSqlString "2017-01-20T22:53:55"
  ev <- newEvent repo "Test Event" startDate Nothing "Just a little test." "at home"
  runEitherT $ writeItem ev

testReadEvent = do
  let repo = Repository "data/calendar"
  liftM (either (const []) id) $ runEitherT $ getEventsForMonth repo 2017 1

testChangeEvent = do
  (event : _) <- testReadEvent
  now <- getCurrentTime
  let changedEvent = event { evStartDate = now }
  runEitherT $ do
    deleteItem event
    writeItem changedEvent
