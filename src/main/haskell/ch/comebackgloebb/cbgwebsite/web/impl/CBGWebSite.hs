{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, MultiParamTypeClasses, ViewPatterns, RecordWildCards #-}

module ch.comebackgloebb.cbgwebsite.web.impl.CBGWebSite where

-- CBG
import Privileges
import Users
import Repository

-- Yesod
import Yesod
import Yesod.Static
import Text.Hamlet
import Yesod.Auth
import Yesod.Auth.BrowserId
import Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import Network.Wai (pathInfo, responseLBS)
import Network.HTTP.Types (status200)

-- other imports
import Control.Concurrent (MVar, newMVar)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Text.Pandoc (readMarkdown, writeHtml)
import Control.Monad (filterM, liftM)
import Data.List (intercalate)
import Control.Exception (IOException)
import Data.Aeson (encode, object)
import Control.Monad.Trans.Either (runEitherT, left)
import Data.Foldable (foldrM)
import Debug.Trace
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))
import Data.DateTime

staticFiles "static"

data CBGWebSite = CBGWebSite { getStatic    :: Static
                             , getSem       :: MVar Bool
                             , httpManager  :: Manager
                             , contentRepo  :: Repository
                             , memberRepo   :: Repository
                             , calendarRepo :: Repository
                             }

mkYesod "CBGWebSite" [parseRoutes|
    /                                RootR                 GET
    /favicon.ico                     FavR                  GET
    /static                          StaticR               Static          getStatic
    /auth                            AuthR                 Auth            getAuth
    /members                         MembersR              GET
    /content/+ContentPath            ContentR              GET
    /members/calendar/#Int/#Int      MemberCalendarR       GET
    /members/event/edit/#Text        EventR                GET POST
    /members/list                    MemberListR           GET
    /members/list/edit/#Text         MemberR               GET POST
|]

instance Yesod CBGWebSite where
    defaultLayout                          = cbgLayout
    approot                                = ApprootStatic ""
    -- isAuthorized route isWriteRequest? = ...
    isAuthorized RootR                 False = return Authorized
    isAuthorized FavR                  False = return Authorized
    isAuthorized (AuthR _)             _     = return Authorized
    isAuthorized (MemberCalendarR _ _) False = return Authorized
    isAuthorized (EventR _)            _     = return Authorized
    isAuthorized MemberListR           False = return Authorized
    isAuthorized (MemberR _)           _     = return Authorized
    isAuthorized MembersR              False = do
      authUser <- maybeAuthId
      case authUser of
        Just userName | userName `has` Read `On` Members -> return Authorized
        _                                                -> return $ Unauthorized ""
    isAuthorized (ContentR _)  False       = return Authorized
    isAuthorized _             _           = return $ Unauthorized ""

instance RenderMessage CBGWebSite FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth CBGWebSite where
    type AuthId CBGWebSite           = Text
    getAuthId                        = return . Just . credsIdent
    loginDest _                      = MembersR
    logoutDest _                     = MembersR
    authPlugins _                    = [ authBrowserId def ]
    authHttpManager                  = httpManager
    maybeAuthId                      = lookupSession "_ID"

--instance YesodJQuery CBGWebSite

emptyWidget :: a -> Widget
emptyWidget _ = [whamlet||]

cbgLayout :: Widget -> Handler Html
cbgLayout widget = do pageContent  <- widgetToPageContent widget
                      maybeAuthId  <- maybeAuthId
                      req          <- getRequest
                      let path     =  map unpack $ pathInfo $ reqWaiRequest req
                      let repoPath =  tail path
                      app          <- getYesod
                      eitherNode   <- liftIO $ runEitherT (getNode (contentRepo app) repoPath)
                      navi         <- widgetToPageContent $ either emptyWidget navigationWidget eitherNode
                      trail        <- widgetToPageContent $ either emptyWidget auditTrail eitherNode
                      withUrlRenderer $(hamletFile "layout.hamlet")

withJQuery :: Widget -> Widget
withJQuery widget = do
    toWidgetHead [hamlet|
        <script src=//ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js>
        <script src=@{StaticR jquery_blockUI_js}>
    |]
    widget

withAngularController :: String -> Widget -> Widget
withAngularController controller widget = do
    let controllerjs = "/static/" ++ controller ++ ".js"
    [whamlet|
        <div ng-app=cbgApp>
            <div ng-controller=#{controller}>
                ^{widget}
    |]
    toWidgetHead [hamlet|
        <script src=//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js>
        <script src=#{controllerjs}>
    |]

getRootR :: Handler ()
getRootR = redirect ("/content/welcome" :: String)

getFavR :: Handler ()
getFavR = sendFile "image/png" "static/cbg-favicon.png"

getMembersR :: Handler Html
getMembersR = defaultLayout [whamlet|<h1>Welcome to the members area|]

data ContentPath = ContentPath [Text]
    deriving (Read, Show, Eq)

instance PathMultiPiece ContentPath where
    toPathMultiPiece (ContentPath pieces) = pieces
    fromPathMultiPiece = Just . ContentPath . filter (/= "..")

getContentR :: ContentPath -> Handler Html
getContentR (ContentPath pieces) = defaultLayout $ do
    app <- getYesod
    eitherNode <- liftIO $ runEitherT $ do
        let url = map unpack pieces
        node <- getNode (contentRepo app) $ trace (show url) url
        return node
    case eitherNode of
        Left ioe -> notFound
        Right node | node_path node == [] -> redirect ("/content/welcome" :: String)
        Right node -> do
            let prop = case getProperty node "text.md" of
                           Just p  -> show $ prop_value p
                           Nothing -> ""
            toWidget $ writeHtml def $ readMarkdown def prop

navigationWidget :: Node -> Widget
navigationWidget node = do eitherNodes <- liftIO $ runEitherT $ do
                               parent       <- getParentNode node
                               siblingNames <- getChildNodeNames parent
                               siblings     <- mapM (getChildNode parent) siblingNames
                               childNames   <- getChildNodeNames node
                               children     <- mapM (getChildNode node) childNames
                               welcome      <- getNode (node_repo node) ["welcome"]
                               return (parent, siblings, children, welcome)
                           case eitherNodes of
                               Left ioe                                    -> trace (show node ++ "/navigationWidget: " ++ show (ioe :: IOException)) (return ())
                               Right (parent, siblings, children, welcome) -> [whamlet|
                                   <li .menu-123 .expanded>
                                       $if node_path parent == []
                                           <a href=#{url welcome} title=#{title welcome}>#{title welcome}
                                       $else
                                           <a href=#{url parent} title=#{title parent}>#{title parent}
                                       <ul .menu>
                                           $forall entry <- siblings
                                               $if entry == welcome
                                               $elseif entry == node
                                                   <li .menu-123 .expanded>
                                                       <a href=#{url entry} title=#{title entry}>#{title entry}
                                                       <ul .menu>
                                                           $forall child <- children
                                                               <li .menu-123>
                                                                   <a href=#{url child} title=#{title child}>#{title child}
                                               $else
                                                   <li .menu-123 .collapsed>
                                                       <a href=#{url entry} title=#{title entry}>#{title entry}
                               |]
    where url           = ("/content/" ++) . urlToString . node_path
          maybeTitleVal = fmap prop_value . flip getProperty ("title" :: String)
          title n       = case maybeTitleVal n of
                              Just v  -> show v
                              Nothing -> ""

auditTrail :: Node -> Widget
auditTrail node = do eitherNodes <- liftIO $ runEitherT getTrail
                     case eitherNodes of
                         Left ioe    -> trace (show node ++ "/auditTrail: " ++ show (ioe :: IOException)) (return ())
                         Right nodes -> mapM_ encodeTrail nodes
    where encodeTrail n = [whamlet|<li .menu-123 .collapsed>
                                       <a href=#{url n} title=#{title n}>#{title n}
                          |]
          url           = ("/content/" ++) . urlToString . node_path
          maybeTitleVal = fmap prop_value . flip getProperty ("title" :: String)
          title n       = case maybeTitleVal n of
                              Just v  -> show v
                              Nothing -> ""
          getTrail :: RepositoryContext [Node]
          getTrail      = do nodes <- foldrM appendToNodes [node] $ node_path node
                             let nodes' = tail nodes -- omit root node
                             if nodes' == []
                             then left $ userError "no root"
                             else do
                                welcome <- getNode (node_repo node) (urlFromString "/welcome")
                                let first = node_path $ head nodes'
                                case first of ["welcome"] -> return nodes'
                                              _           -> return (welcome : nodes')
          appendToNodes :: PathComponent -> [Node] -> RepositoryContext [Node]
          appendToNodes _ (n : ns) = do p <- getParentNode n
                                        return (p : n : ns)


------------------------------------------------------------------------------------------
--- Member Calendar
------------------------------------------------------------------------------------------

data Event = Event { ev_startDate   :: DateTime
                   , ev_endDate     :: DateTime
                   , ev_description :: Text
                   } deriving (Show)

instance Persistent Event where
    fromNode node = Event (dateProperty "startDate")
                          (dateProperty "endDate")
                          (textProperty "description")
      where textProperty =                         pack          . show . fromMaybe (StringValue "") . liftM prop_value . getProperty node -- we should be throwing here in case of error
            dateProperty = fromMaybe startOfTime . fromSqlString . show . fromMaybe (StringValue "") . liftM prop_value . getProperty node -- we should be throwing here in case of error
    toNode repo eventName event = Node eventName
                                         (urlFromString eventName)
                                         [ (Property "startDate"   $ StringValue $ toSqlString $ ev_startDate   event)
                                         , (Property "endDate"     $ StringValue $ toSqlString $ ev_endDate     event)
                                         , (Property "description" $ StringValue $ unpack      $ ev_description event)
                                         ]
                                         repo

instance ToJSON Event where
    toJSON Event {..} = object [ "startDate"   .= toSqlString ev_startDate
                               , "endDate"     .= toSqlString ev_endDate
                               , "description" .=             ev_description
                               ]

getMemberCalendarR :: Int -> Int -> Handler TypedContent
getMemberCalendarR year month = if month < 1 || month > 12
                                then notFound
                                else selectRep $ do
    provideRep $ renderEvents $ \events -> defaultLayout
        [whamlet|
            <table>
                <tr>
                    <th>Startdatum
                    <th>Enddatum
                    <th>Beschreibung
                $forall event <- events
                    <tr>
                        <td>#{toSqlString $ ev_startDate   event}
                        <td>#{toSqlString $ ev_endDate     event}
                        <td>^{renderDesc  $ ev_description event}
                        <td>
                            <a href=@{EventR $ eventId event}>
                                <button>Edit
        |]
    provideRep $ renderEvents returnJson
  where renderEvents :: HasContentType a => ([Event] -> Handler a) -> Handler a
        renderEvents as = do
              app           <- getYesod
              eitherEvents  <- liftIO $ runEitherT $ getNode (calendarRepo app) url >>= getEventList
              case eitherEvents of
                  Left  e       -> do $logError $ pack $ show e
                                      as ([] :: [Event])
                  Right events -> as events
        url = map (pathCompFromString . show) [year, month]
        getEventList :: Node -> RepositoryContext [Event]
        getEventList node = getChildNodes node >>= return . (map fromNode)
        renderDesc desc = writeHtml def $ readMarkdown def $ unpack desc
        eventId event = pack $ intercalate "/" [show year, show month, toSqlString $ ev_startDate event]
            where (year, month, _) = toGregorian' $ ev_startDate event

eventForm :: Maybe Event -> Html -> MForm Handler (FormResult Event, Widget)
eventForm mevent = renderDivs $ makeEvent
    <$> areq textField "Startdatum"   ((pack . toSqlString . ev_startDate)  <$> mevent)
    <*> areq textField "Enddatum"     ((pack . toSqlString . ev_endDate)    <$> mevent)
    <*> areq textField "Beschreibung" (                      ev_description <$> mevent)
  where makeEvent start end desc = Event (fromMaybe startOfTime $ fromSqlString $ unpack start)
                                         (fromMaybe startOfTime $ fromSqlString $ unpack end)
                                         desc

getEventR :: Text -> Handler Html
getEventR name = do app               <- getYesod
                    eitherEvent       <- liftIO $ readItem (calendarRepo app) (unpack name)
                    let mevent        =  either (\e -> trace (show e) Nothing)
                                                Just
                                                eitherEvent
                    (widget, encType) <- generateFormPost $ eventForm mevent
                    defaultLayout [whamlet|
                       <form method=post action=@{EventR name} enctype=#{encType}>
                           ^{widget}
                           <button>Submit
                    |]

postEventR :: Text -> Handler Html
postEventR name = do
    ((result, widget), enctype) <- runFormPost $ eventForm Nothing
    case result of FormSuccess event -> do app               <- getYesod
                                           let strName       =  unpack name
                                           result <- liftIO $ writeItem (calendarRepo app) strName event
                                           case result of
                                               Left e -> return $ trace (show e) ()
                                               _      -> return $ ()
                                           let (year, month, _) = toGregorian' $ ev_startDate event
                                           redirect $ MemberCalendarR (fromInteger year) month
                   _                 -> defaultLayout [whamlet|
                                            <p>Da stimmt etwas nicht, versuch's nochmal
                                            <form method=post action=@{EventR name} enctype=#{enctype}>
                                                ^{widget}
                                                <button>Submit
                                        |]


------------------------------------------------------------------------------------------
--- Member List
------------------------------------------------------------------------------------------

data Member = Member { vorname :: Text
                     , name    :: Text
                     , strasse :: Text
                     , ort     :: Text
                     } deriving (Show)

instance ToJSON Member where
    toJSON Member {..} = object [ "name"     .= name
                                , "vorname"  .= vorname
                                , "strasse"  .= strasse
                                , "ort"      .= ort
                                ]

instance Persistent Member where
    fromNode node = Member (property "firstname")
                           (property "lastname")
                           (property "address")
                           (property "city")
      where property = pack . show . fromMaybe (StringValue "") . liftM prop_value . getProperty node
    toNode repo memberName member = Node memberName
                                         (urlFromString memberName)
                                         [ (Property "firstname" $ StringValue $ unpack $ vorname member)
                                         , (Property "lastname"  $ StringValue $ unpack $ name member)
                                         , (Property "address"   $ StringValue $ unpack $ strasse member)
                                         , (Property "city"      $ StringValue $ unpack $ ort member)
                                         ]
                                         repo

memberForm :: Maybe Member -> Html -> MForm Handler (FormResult Member, Widget)
memberForm mmember = renderDivs $ Member
    <$> areq textField "Vorname" (vorname <$> mmember)
    <*> areq textField "Name"    (name    <$> mmember)
    <*> areq textField "Strasse" (strasse <$> mmember)
    <*> areq textField "Ort"     (ort     <$> mmember)

getMemberR :: Text -> Handler Html
getMemberR name = do app               <- getYesod
                     eitherMember      <- liftIO $ readItem (memberRepo app) (unpack name)
                     let mmember       =  either (\e -> trace (show e) Nothing)
                                                 Just
                                                 eitherMember
                     (widget, encType) <- generateFormPost $ memberForm mmember
                     defaultLayout [whamlet|
                        <form method=post action=@{MemberR name} enctype=#{encType}>
                            ^{widget}
                            <button>Submit
                     |]

postMemberR :: Text -> Handler Html
postMemberR name = do 
    ((result, widget), enctype) <- runFormPost $ memberForm Nothing
    case result of FormSuccess member -> do app               <- getYesod
                                            result <- liftIO $ writeItem (memberRepo app) (unpack name) member
                                            case result of
                                                Left e -> return $ trace (show e) ()
                                                _      -> return $ ()
                                            redirect MemberListR
                   _                  -> defaultLayout [whamlet|
                                             <p>Da stimmt etwas nicht, versuch's nochmal
                                             <form method=post action=@{MemberR name} enctype=#{enctype}>
                                                 ^{widget}
                                                 <button>Submit
                                         |]

getMemberListR :: Handler TypedContent
getMemberListR = selectRep $ do
    provideRep $ renderMembers $ \members ->
                defaultLayout [whamlet|
                    <table>
                        <tr>
                            <th>Vorname
                            <th>Name
                            <th>Strasse
                            <th>Ort
                        $forall member <- members
                            <tr>
                                <td>#{vorname member}
                                <td>#{name member}
                                <td>#{strasse member}
                                <td>#{ort member}
                                <td>
                                    <a href=@{MemberR $ memberId member}>
                                        <button>Edit
                |]
    provideRep $ renderMembers returnJson
  where renderMembers :: HasContentType a => ([Member] -> Handler a) -> Handler a
        renderMembers as = do
              app           <- getYesod
              eitherMembers <- liftIO $ runEitherT $ getNode (memberRepo app) (urlFromString "/") >>= getMemberList
              case eitherMembers of
                  Left  e       -> do $logError $ pack $ show e
                                      as ([] :: [Member])
                  Right members -> as members
        memberId member = T.concat [vorname member, (pack " "), name member]
        getMemberList :: Node -> RepositoryContext [Member]
        getMemberList node = getChildNodes node >>= return . (map fromNode)
