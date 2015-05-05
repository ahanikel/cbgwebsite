{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, MultiParamTypeClasses, ViewPatterns #-}

--module CBGWebSite (CBGWebSite(..)) where
module CBGWebSite where

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
import Text.Pandoc (readMarkdown, writeHtml)
import Control.Monad (filterM)
import Data.List (intercalate)
import Control.Exception (IOException)
import Data.Aeson (encode, object)
import Control.Monad.Trans.Either (runEitherT, left)
import Data.Foldable (foldrM)
import Debug.Trace

staticFiles "static"

data CBGWebSite = CBGWebSite { getStatic   :: Static
                             , getSem      :: MVar Bool
                             , httpManager :: Manager
                             , repo        :: Repository
                             }

mkYesod "CBGWebSite" [parseRoutes|
    /                            RootR                 GET
    /favicon.ico                 FavR                  GET
    /static                      StaticR               Static          getStatic
    /auth                        AuthR                 Auth            getAuth
    /mitglieder                  MembersR              GET
    /content/+ContentPath        ContentR              GET
    /mitglieder/kalender         MemberCalendarR       GET
    /mitglieder/kalender.json    MemberCalendarJsonR   GET
    /mitglieder/liste            MemberListR           GET
    /mitglieder/liste.json       MemberListJsonR       GET
|]

instance Yesod CBGWebSite where
    defaultLayout                          = cbgLayout
    approot                                = ApprootStatic ""
    -- isAuthorized route isWriteRequest? = ...
    isAuthorized RootR               False = return Authorized
    isAuthorized FavR                False = return Authorized
    isAuthorized (AuthR _)           _     = return Authorized
    isAuthorized MemberCalendarJsonR False = return Authorized
    isAuthorized MemberListR         False = return Authorized
    isAuthorized MemberListJsonR     False = return Authorized
    isAuthorized MembersR            False = do
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

emptyWidget :: a -> Widget
emptyWidget _ = [whamlet||]

cbgLayout :: Widget -> Handler Html
cbgLayout widget = do pageContent  <- widgetToPageContent widget
                      maybeAuthId  <- maybeAuthId
                      req          <- getRequest
                      let path     =  map unpack $ pathInfo $ reqWaiRequest req
                      let repoPath =  tail path
                      app          <- getYesod
                      eitherNode   <- liftIO $ runEitherT (getNode (repo app) repoPath)
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
        node <- getNode (repo app) url
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
    where url           = ("/content" ++) . urlToString . node_path
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
          url           = ("/content" ++) . urlToString . node_path
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

getMemberCalendarR :: Handler Html
getMemberCalendarR = defaultLayout $ withAngularController "MemberCalendarController" $ do
    [whamlet|
        <table>
            <tr>
                <th ng-repeat="col in ['zeit', 'name', 'vorname', 'strasse', 'ort']">{{col}}
            <tr ng-repeat="item in memberCalendarItems">
                <td>{{item.zeit}}
                <td>{{item.name}}
                <td>{{item.vorname}}
                <td>{{item.strasse}}
                <td>{{item.ort}}
    |]

getMemberCalendarJsonR :: Handler ()
getMemberCalendarJsonR = sendWaiResponse $ responseLBS
                             status200
                             [("Content-Type", "application/json")]
                             "{ 'hello': 'world' }"

getMemberListR :: Handler Html
getMemberListR = defaultLayout $ withAngularController "MemberListController" $ do
    [whamlet|
        <table>
            <tr>
                <th ng-repeat="col in ['vorname', 'name', 'strasse', 'ort']">{{col}}
            <tr ng-repeat="item in memberListItems">
                <td>{{item.vorname}}
                <td>{{item.name}}
                <td>{{item.strasse}}
                <td>{{item.ort}}
    |]

getMemberListJsonR :: Handler ()
getMemberListJsonR = sendWaiResponse $ responseLBS
                             status200
                             [("Content-Type", "application/json")]
                             $ encode [ object [ "name"     .= pack "Meier"
                                               , "vorname"  .= pack "Fritz"
                                               , "strasse"  .= pack "Musterweg 23"
                                               , "ort"      .= pack "9999 Testheim"
                                               ]
                                      ]
