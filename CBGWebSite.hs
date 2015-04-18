{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, MultiParamTypeClasses, ViewPatterns #-}

module CBGWebSite (CBGWebSite(..)) where

-- CBG
import Privileges
import Users

-- Yesod
import Yesod
import Yesod.Static
import Text.Hamlet
import Yesod.Auth
import Yesod.Auth.BrowserId
import Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import Network.Wai (pathInfo)

-- other imports
import Control.Concurrent (MVar, newMVar)
import Data.Text (Text, unpack, pack)
import Text.Pandoc (readMarkdown, writeHtml)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Monad (filterM)
import Data.List (intercalate)
import System.FilePath ((</>), normalise, splitDirectories)
import Control.Exception (catch, SomeException)

staticFiles "static"

data CBGWebSite = CBGWebSite { getStatic   :: Static
                             , getSem      :: MVar Bool
                             , httpManager :: Manager
                             }

mkYesod "CBGWebSite" [parseRoutes|
    /                     RootR           GET
    /favicon.ico          FavR            GET
    /static               StaticR         Static          getStatic
    /auth                 AuthR           Auth            getAuth
    /mitglieder           MembersR        GET
    /content/+ContentPath ContentR        GET
|]

instance Yesod CBGWebSite where
    defaultLayout                    = cbgLayout
    approot                          = ApprootStatic ""
    -- isAuthorized route isWriteRequest? = ...
    isAuthorized RootR         False = return Authorized
    isAuthorized FavR          False = return Authorized
    isAuthorized (AuthR _)     _     = return Authorized
    isAuthorized MembersR      False = do
        authUser <- maybeAuthId
        case authUser of
            Just userName | userName `has` Read `On` Members -> return Authorized
            _                                                -> return $ Unauthorized ""
    isAuthorized (ContentR _)  False = return Authorized
    isAuthorized _             _     = return $ Unauthorized ""

instance RenderMessage CBGWebSite FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth CBGWebSite where
    type AuthId CBGWebSite           = Text
    getAuthId                        = return . Just . credsIdent
    loginDest _                      = RootR
    logoutDest _                     = RootR
    authPlugins _                    = [ authBrowserId def ]
    authHttpManager                  = httpManager
    maybeAuthId                      = lookupSession "_ID"

cbgLayout :: Widget -> Handler Html
cbgLayout widget = do pageContent  <- widgetToPageContent widget
                      maybeAuthId  <- maybeAuthId
                      req          <- getRequest
                      let path     =  map unpack $ pathInfo $ reqWaiRequest req
                      let naviRoot =  intercalate "/" path
                      naviEntries  <- liftIO $ getContentNavi naviRoot
                      navi         <- widgetToPageContent $ contentNavi naviRoot
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
    [whamlet|
        <div ng-app=cbgApp>
            <div ng-controller=#{controller}>
                ^{widget}
    |]
    toWidgetHead [hamlet|
        <script src=//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js>
        <script src=@{StaticR controllers_js}>
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
    let sourceFileName = "content/" ++ intercalate "/" (map unpack pieces) ++ "/text.md"
    markdownSource <- liftIO $ readFile sourceFileName
    let html = writeHtml def $ readMarkdown def markdownSource
    toWidget html

data Node = Node { ct_name  :: String
                 , ct_title :: String
                 , ct_url   :: String
                 }
    deriving (Read, Show, Eq)

getNode :: FilePath -> IO Node
getNode path = do let normalisedPath = normalise path
                      splitPath      = splitDirectories normalisedPath
                      name           = last splitPath
                      url            = "/" ++ normalisedPath
                  title <- catch (readFile $ normalisedPath </> "title")
                                 ((\_ -> return "") :: SomeException -> IO String)
                  return $ Node name title url

getContentNavi :: FilePath -> IO [Node]
getContentNavi root = getDirectoryContents root >>= filterM acceptable >>= mapM toNode
    where acceptable item = do isDir <- doesDirectoryExist $ root </> item
                               let noDot = item `notElem` [".", ".."]
                               return $ isDir && noDot
          toNode item = getNode $ root </> item

contentNavi :: FilePath -> Widget
contentNavi path = do current  <- liftIO $ getNode path
                      parent   <- liftIO $ getNode (path ++ "/..")
                      siblings <- liftIO $ getContentNavi $ tail $ ct_url parent
                      children <- liftIO $ getContentNavi path
                      [whamlet|
                          $forall entry <- siblings
                              <li .menu-123>
                                  <a href=#{ct_url entry} title=#{ct_title entry}>#{ct_title entry}
                              $if entry == current
                                  <ul>
                                  $forall child <- children
                                      <li .menu-123>
                                          <a href=#{ct_url child} title=#{ct_title child}>#{ct_title child}
                      |]
