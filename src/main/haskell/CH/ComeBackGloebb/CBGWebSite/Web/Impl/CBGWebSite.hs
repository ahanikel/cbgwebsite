{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE ViewPatterns               #-}

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.CBGWebSite where

-- CBG
import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Asset
import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Gallery
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.Privileges
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.Users

-- Yesod
import           Network.HTTP.Conduit                              (Manager)
import           Network.HTTP.Types                                (status200)
import           Network.Wai                                       (responseLBS)
import           Text.Hamlet
import           Yesod                                             hiding
                                                                    (deleteBy,
                                                                    joinPath)
import           Yesod.Auth
import           Yesod.Auth.Account
import           Yesod.Auth.GoogleEmail2
import           Yesod.Static

-- other imports
import           Control.Concurrent                                (MVar)
import           Control.Exception                                 (IOException)
import           Control.Exception.Base                            (throwIO)
import           Control.Monad                                     (liftM)
import           Control.Monad.Trans.Either                        (left,
                                                                    runEitherT)
import           Data.ByteString                                   (ByteString)
import qualified Data.ByteString.UTF8                              as U8
import           Data.Conduit
import qualified Data.Conduit.Binary                               as CB
import           Data.DateTime
import           Data.Foldable                                     (foldrM)
import           Data.List                                         (deleteBy,
                                                                    elemIndex,
                                                                    intercalate,
                                                                    sort)
import           Data.Maybe                                        (fromMaybe)
import           Data.Text                                         (Text)
import qualified Data.Text                                         as T
import qualified Data.Text.Lazy                                    as TL
import           Database.Persist.Sqlite                           (ConnectionPool,
                                                                    SqlBackend,
                                                                    runSqlPool)
import           Debug.Trace
import           Network.Mail.Mime

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
User
    username Text
    UniqueUsername username
    password ByteString
    emailAddress Text
    verified Bool
    verifyKey Text
    resetPasswordKey Text
    deriving Show
|]

staticFiles "src/main/haskell/CH/ComeBackGloebb/CBGWebSite/Web/static"

data CBGWebSite = CBGWebSite { getStatic    :: Static
                             , getSem       :: MVar Bool
                             , httpManager  :: Manager
                             , contentRepo  :: Repository
                             , memberRepo   :: Repository
                             , calendarRepo :: Repository
                             , galleryRepo  :: Repository
                             , assetRepo    :: Repository
                             , clientId     :: Text
                             , clientSecret :: Text
                             , dbPool       :: ConnectionPool
                             }

mkYesod "CBGWebSite" [parseRoutes|
    /                                RootR                 GET
    /favicon.ico                     FavR                  GET
    /static                          StaticR               Static          getStatic
    /auth                            AuthR                 Auth            getAuth
    /members                         MembersR              GET
    /content/+ContentPath            ContentR              GET
    /edit/content/+ContentPath       EditContentR          GET POST
    /members/calendar                MemberCalendarR       GET
    /members/calendar/#Int/#Int      MemberCalendarMR      GET
    /members/event/edit/#Text        EventR                GET POST
    /members/list                    MemberListR           GET
    /members/list/edit/#Text         MemberR               GET POST
    /galleries                       GalleriesR            GET
    /gallery/#Text                   GalleryR              GET POST DELETE
    /gallery/#Text/images            GalleryImagesR        GET
    /gallery/#Text/image/#Text       GalleryImageR         GET POST DELETE
    /image/#Text/#Text               ImageR                GET
    /upload/image/#Text              UploadImageR          POST
    /asset/+ContentPath              AssetR                GET
|]

instance Yesod CBGWebSite where
    defaultLayout                             = cbgLayout []
    approot                                   = ApprootStatic "https://test.comebackgloebb.ch"
    -- isAuthorized route isWriteRequest? = ...
    isAuthorized RootR                  False = return Authorized
    isAuthorized FavR                   False = return Authorized
    isAuthorized (AuthR _)              _     = return Authorized
    isAuthorized (EventR _)             _     = return $ Unauthorized ""

    -- the members area
    isAuthorized MembersR               False = do
      authUser <- maybeAuthId
      case authUser of
        Just userName | userName `has` Read  `On` Members -> return Authorized
        _                                                 -> return $ Unauthorized ""

    -- the member list
    isAuthorized MemberListR            False = do
      authUser <- maybeAuthId
      case authUser of
        Just userName | userName `has` Read  `On` MemberList -> return Authorized
        _                                                    -> return $ Unauthorized ""

    isAuthorized MemberListR            True  = do
      authUser <- maybeAuthId
      case authUser of
        Just userName | userName `has` Write `On` MemberList -> return Authorized
        _                                                    -> return $ Unauthorized ""

    isAuthorized (MemberR _)            w     = isAuthorized MemberListR w

    -- the event calendar
    isAuthorized  MemberCalendarR       False = isAuthorized MembersR False

    isAuthorized (MemberCalendarMR _ _) False = isAuthorized MembersR False

    isAuthorized (MemberCalendarMR _ _) True  = do
      authUser <- maybeAuthId
      case authUser of
        Just userName | userName `has` Write `On` MemberCalendar -> return Authorized
        _                                                        -> return $ Unauthorized ""

    isAuthorized (ContentR _)           False = return Authorized

    isAuthorized (EditContentR _)       w     = isAuthorized MemberListR w

    -- the galleries
    isAuthorized GalleriesR             False = isAuthorized MembersR False

    isAuthorized (GalleryR _)           False = isAuthorized MembersR False

    isAuthorized (GalleryImagesR _)     False = isAuthorized MembersR False

    isAuthorized (GalleryImageR _ _)    False = isAuthorized MembersR False

    isAuthorized (ImageR _ _)           False = isAuthorized MembersR False

    isAuthorized (UploadImageR _)       True  = isAuthorized MembersR False

    -- the assets
    isAuthorized (AssetR _)             False = isAuthorized MembersR False

    -- everything else
    isAuthorized _                      _     = return $ Unauthorized ""

instance RenderMessage CBGWebSite FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth CBGWebSite where
    type AuthId CBGWebSite           = Text
    getAuthId                        = return . Just . credsIdent
    loginDest _                      = MembersR
    logoutDest _                     = RootR
    authPlugins self                 = [ authGoogleEmail (clientId self) (clientSecret self), accountPlugin ]
    authHttpManager                  = httpManager
    maybeAuthId                      = lookupSession "_ID"

instance YesodPersist CBGWebSite where
    type YesodPersistBackend CBGWebSite = SqlBackend
    runDB action = do
        pool <- liftM dbPool getYesod
        runSqlPool action pool

instance AccountSendEmail CBGWebSite where
    sendVerifyEmail uname email url = do
        $(logInfo) $ T.concat [ "Verification email for "
                              , uname
                              , " (", email, "): "
                              , url
                              ]
        liftIO $ renderSendMail $
          simpleMail' (Address (Just uname) email)
                      (Address (Just "Come Back Glöbb") "info@comebackgloebb.ch")
                      (T.concat ["Verification email for ", uname])
                      (TL.concat ["In order to verify your email address, please click this link: ", TL.fromStrict url])

    sendNewPasswordEmail uname email url = do
        $(logInfo) $ T.concat [ "Reset password email for "
                              , uname
                              , " (", email, "): "
                              , url
                              ]
        liftIO $ renderSendMail $
          simpleMail' (Address (Just uname) email)
                      (Address (Just "Come Back Glöbb") "info@comebackgloebb.ch")
                      (T.concat ["Password reset request for ", uname])
                      (TL.concat ["We have received a password reset request for your account. If you didn't request a password reset, please ignore this email. If the request was indeed yours, please click the link in order to change your password.: ", TL.fromStrict url])


instance YesodAuthAccount (AccountPersistDB CBGWebSite User) CBGWebSite where
    runAccountDB = runAccountPersistDB

instance PersistUserCredentials User where
    userUsernameF = UserUsername
    userPasswordHashF = UserPassword
    userEmailF = UserEmailAddress
    userEmailVerifiedF = UserVerified
    userEmailVerifyKeyF = UserVerifyKey
    userResetPwdKeyF = UserResetPasswordKey
    uniqueUsername = UniqueUsername

    userCreate name email key pwd = User name pwd email False key ""

cbgLayout :: [Text] -> Widget -> Handler Html
cbgLayout path widget = do pageContent  <- widgetToPageContent widget
                           maybeAuthId' <- maybeAuthId
                           navi         <- widgetToPageContent $ navigationWidget maybeAuthId' path
                           trail        <- widgetToPageContent $ auditTrail       path
                           withUrlRenderer $(hamletFile "src/main/haskell/CH/ComeBackGloebb/CBGWebSite/Web/Impl/layout.hamlet")

withJQuery :: Widget -> Widget
withJQuery widget = do
    toWidgetHead [hamlet|
        <script src=//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js>
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

withTinyMCE :: Widget -> Widget
withTinyMCE widget = do
    [whamlet|
        <textarea #body name=body>
            ^{widget}
    |]
    toWidgetHead [hamlet|
        <script src=//cdn.tinymce.com/4/tinymce.min.js>
        <script>
            tinymce.init({ selector:'textarea' });
    |]

ckEditor :: Widget -> Widget
ckEditor widget = do
    [whamlet|
        <textarea #body name=body>
            ^{widget}
        <script>
            CKEDITOR.replace('body');
   |]
    toWidgetHead [hamlet|
        <script src=//cdn.ckeditor.com/4.5.7/standard/ckeditor.js>
    |]

withBootstrap :: Widget -> Widget
withBootstrap widget = do
    withJQuery widget
    toWidgetHead [hamlet|
        <!-- Latest compiled and minified CSS -->
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" integrity="sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7" crossorigin="anonymous">

        <!-- Optional theme -->
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css" integrity="sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r" crossorigin="anonymous">

        <!-- Latest compiled and minified JavaScript -->
        <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js" integrity="sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS" crossorigin="anonymous">
    |]

editPage :: Widget -> Widget
editPage contentBody = [whamlet|
    <div #buttons>
        <button #savebutton type=button .btn .btn-lg .btn-primary>
            Save
    <script>
        function restoreSaveButton() {
            \$('#savebutton').removeClass('btn-success')
                            .removeClass('btn-danger')
                            .addClass('btn-primary')
                            .text('Save');
        }
        function successSaveButton() {
            \$('#savebutton').removeClass('btn-primary')
                            .removeClass('btn-danger')
                            .addClass('btn-success')
                            .text('Saved!');
            window.setTimeout(restoreSaveButton, 5000);
        }
        function errorSaveButton() {
            \$('#savebutton').removeClass('btn-success')
                            .removeClass('btn-primary')
                            .addClass('btn-danger')
                            .text('Error while saving! Retry?');
            window.setTimeout(restoreSaveButton, 5000);
        }
        \$('#savebutton').click(function(event) {
            \$.post('#', { body: CKEDITOR.instances.body.getData() }, function(ret) {})
            .success(successSaveButton)
            .error(errorSaveButton);
        });
    <div>
        <ul .nav .nav-tabs role=tablist>
            <li role=presentation .active>
                <a href=#content aria-controls=content role=tab data-toggle=tab>Content
            <li role=presentation>
                <a href=#properties aria-controls=properties role=tab data-toggle=tab>Properties
            <li role=presentation>
                <a href=#permissions aria-controls=permissions role=tab data-toggle=tab>Permissions
        <div .tab-content>
            <div role=tabpanel .tab-pane .active id=content>
                ^{ckEditor contentBody}
            <div role=tabpanel .tab-pane .active id=properties>...
            <div role=tabpanel .tab-pane .active id=permissions>...
    |]

editLayout :: [Text] -> Widget -> Handler Html
editLayout path = cbgLayout ("edit" : path) . withBootstrap . editPage

getRootR :: Handler ()
getRootR = redirect ("/content/welcome" :: String)

getFavR :: Handler ()
getFavR = sendFile "image/png" "src/main/haskell/CH/ComeBackGloebb/CBGWebSite/Web/static/cbg-favicon.png"

getMembersR :: Handler Html
getMembersR = cbgLayout ["members"] [whamlet|
  <h1>Willkommen auf der neuen Come Back Glöbb Homepage!
  <p>Auf den ersten Blick sieht alles so aus wie vorher, aber das täuscht :-)
  <p>Vorteile:
    <ul>
      <li>Man kann Seiten jetzt mit einem Wysiwyg-Editor bearbeiten. Einfach in der Browser-URL vor "/content/..." ein "/edit" setzen, also z.B. <tt>https://www.comebackgloebb.ch/edit/content/welcome.html</tt>
      <li>Sicherere Speicherung der Passwörter (sha512 statt dem unsicheren md5).
      <li>Der Upload von Bildern sollte einfacher sein (aber vielleicht ist das browserabhängig, Feedback willkommen!).
      <li>Die Mitgliederliste ist online bearbeitbar.
      <li>Es gibt einen Kalender, in den man Termine eintragen kann.
  <p>(vorläufige) Nachteile:
    <ul>
      <li>Der Kalender funktioniert noch nicht.
      <li>Man kann noch keine Sachen hochladen ausser Bildern.
      <li>Die Bilder werden in der Galerie immer noch in Originalgrösse runtergeladen, statt als kleinerer Thumbnail.
      <li>Allgemein sehen einige Sachen noch etwas "unfertig" aus.
      <li>Eventuell habe ich noch etwas vergessen, was auf der alten Homepage drauf war?
  <p>Die Negativpunkte sollten nach und nach behoben werden. Fehlermeldungen, Wünsche und Anregungen sind willkommen!
  <p>Damit es immer noch möglich ist, sich mit dem bisherigen Passwort anzumelden, sind die Passwörter noch so lange im unsicheren md5-Format gespeichert, bis sie geändert werden. Nach der Änderung werden sie im sicheren sha256-Format gespeichert. Es wird daher empfohlen, das Passwort möglichst bald zu ändern. Dazu geht man wie folgt vor:
     <ul>
       <li>Sich über "Logout..." abmelden.
       <li>Die "Mitglieder"-Seite anwählen. Es erscheint die Login-Seite.
       <li>Auf "Forgot password" klicken und den Benutzernamen angeben.
       <li>Auf eine E-Mail vom Come Back Glöbb warten und den enthaltenen Link anklicken.
       <li>Das neue Passwort zwei mal eingeben (kann auch dasselbe wie das alte sein). Fertig!
|]

data ContentPath = ContentPath [Text]
    deriving (Read, Show, Eq)

instance PathMultiPiece ContentPath where
    toPathMultiPiece (ContentPath pieces) = pieces
    fromPathMultiPiece = Just . ContentPath . filter (/= "..")

getEditContentR :: ContentPath -> Handler Html
getEditContentR (ContentPath pieces) = do
    app <- getYesod
    eitherNode <- liftIO $ runEitherT $ do
        let url = map T.unpack pieces
        getNode (contentRepo app) url
    (prop, _) <- case eitherNode of
        Left _ -> notFound
        Right node | null (node_path node) -> redirect ("/content/welcome" :: String)
        Right node -> do
            let prop  = case getProperty node "text.html" of
                             Just p  -> show $ prop_value p
                             Nothing -> ""
            let title = case getProperty node "title" of
                             Just p  -> show $ prop_value p
                             Nothing -> ""
            return (prop, title)
    let contentBody = toWidget $ toHtml prop
    -- let propertiesWidget =
    -- let permissionsWidget =
    editLayout ("content" : pieces) contentBody

postEditContentR :: ContentPath -> Handler Html
postEditContentR (ContentPath pieces) = do
    app <- getYesod
    eitherNode <- liftIO $ runEitherT $ do
        let url = map T.unpack pieces
        getNode (contentRepo app) url
    case eitherNode of
        Left _ -> notFound
        Right node | null (node_path node) -> notFound
        Right node -> do
          let props   = node_props node
              props'  = deleteBy (\ (Property a _) (Property b _) -> a == b) (Property "text.html" (StringValue "")) props
          body <- runInputPost $ ireq textField "body"
          let props'' = Property "text.html" (StringValue $ T.unpack body) : props'
          result <- liftIO $ runEitherT $ writeNode $ node { node_props = props'' }
          case result of
            Left ioe -> liftIO $ throwIO ioe
            --Right _  -> redirect $ joinPath ("/content/" : (map unpack pieces))
            Right _ -> withUrlRenderer [hamlet||]

getContentR :: ContentPath -> Handler Html
getContentR (ContentPath pieces) = cbgLayout ("content" : pieces) $ do
    app <- getYesod
    eitherNode <- liftIO $ runEitherT $ do
        let url = map T.unpack pieces
        getNode (contentRepo app) url
    case eitherNode of
        Left _ -> notFound
        Right node | null (node_path node) -> redirect ("/content/welcome" :: String)
        Right node -> do
            let prop = case getProperty node "text.html" of
                           Just p  -> show $ prop_value p
                           Nothing -> ""
            toWidget $ preEscapedToMarkup prop

------------------------------------------------------------------------------------------
--- Navigation
------------------------------------------------------------------------------------------

navigationWidget :: Maybe authId -> [Text] -> Widget
navigationWidget maybeAuthId' path = do
    let path' =  map T.unpack path
    let casePart = case maybeAuthId' of
                       Just _ ->
                           case path' of
                               ("content" : _) ->
                                   [whamlet|
                                               <li .expanded>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                                   ^{contentNavigation path}
                                               <li .collapsed>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                               <li .collapsed>
                                                   <a href=@{GalleriesR} title=Fotoalben>Fotoalben
                                   |]
                               ("members" : _) ->
                                   [whamlet|
                                               <li .collapsed>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                               <li .expanded>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                                   <ul .menu>
                                                       <li .leaf>
                                                           <a href=@{MemberCalendarR} title=Kalender>Kalender
                                                       <li .leaf>
                                                           <a href=@{MemberListR} title=Mitgliederliste>Mitgliederliste
                                                       <li .leaf>
                                                           <a href=@{AssetR $ ContentPath ["Verein", "Statuten", "Statuten.pdf"]} title=Statuten>Statuten
                                               <li .collapsed>
                                                   <a href=@{GalleriesR} title=Fotoalben>Fotoalben
                                   |]
                               ("galleries" : _) ->
                                   [whamlet|
                                               <li .collapsed>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                               <li .collapsed>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                               <li .expanded>
                                                   <a href=@{GalleriesR} title=Fotoalben>Fotoalben
                                                   <ul .menu>
                                                       ^{galleryNavigation path}
                                   |]
                               ("gallery" : _) ->
                                   [whamlet|
                                               <li .collapsed>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                               <li .collapsed>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                               <li .expanded>
                                                   <a href=@{GalleriesR} title=Fotoalben>Fotoalben
                                                   <ul .menu>
                                                       ^{galleryNavigation path}
                                   |]
                               _ ->
                                   [whamlet|
                                               <li .collapsed>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                               <li .collapsed>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                               <li .collapsed>
                                                   <a href=@{GalleriesR} title=Fotoalben>Fotoalben
                                   |]
                       Nothing ->
                               [whamlet|
                                       <li .expanded>
                                           <a href=@{RootR} title=Willkommen>Willkommen
                                           <ul .menu>
                                               ^{contentNavigation path}
                                       <li .leaf .last>
                                           <a href=@{AuthR LoginR} title="Mitglieder">Mitglieder
                               |]
    [whamlet|
        <div #block-menu-primary-links .clear-block .block .block-menu>
            <h1>Come Back Glöbb
            <div .content>
                <ul .menu>
                    ^{casePart}
                    <li .leaf .last>
                        <a href=@{AuthR LogoutR} title="Logout">Logout
    |]

contentNavigation :: [Text] -> Widget
contentNavigation path = do app         <- getYesod
                            eitherNodes <- liftIO $ runEitherT $ do
                                let path'    =  map T.unpack path
                                let repoPath =  case path' of
                                                    ("content" : rest)          -> rest
                                                    ("edit" : "content" : rest) -> rest
                                                    _                           -> fail "no content node"
                                node         <- getNode (contentRepo app) repoPath
                                parent       <- getParentNode node
                                siblingNames <- getChildNodeNames parent
                                siblings     <- mapM (getChildNode parent) siblingNames
                                childNames   <- getChildNodeNames node
                                children     <- mapM (getChildNode node) childNames
                                welcome      <- getNode (node_repo node) ["welcome"]
                                return (node, parent, siblings, children, welcome)
                            case eitherNodes of
                                Left _ ->
                                    return ()
                                Right (node, parent, siblings, children, welcome) -> do
                                    let subnavi = [whamlet|
                                        $forall entry <- siblings
                                            $if entry == welcome
                                            $elseif entry == node
                                                <li .expanded>
                                                    <a href=#{url entry} title=#{title entry}>#{title entry}
                                                    <ul .menu>
                                                        $forall child <- children
                                                            <li>
                                                                <a href=#{url child} title=#{title child}>#{title child}
                                            $else
                                                <li .collapsed>
                                                    <a href=#{url entry} title=#{title entry}>#{title entry}
                                    |]
                                    [whamlet|
                                        <ul .menu>
                                            $if node_path parent == []
                                                ^{subnavi}
                                            $else
                                                <li .expanded>
                                                    <a href=#{url parent} title=#{title parent}>#{title parent}
                                                    <ul .menu>
                                                        ^{subnavi}
                                    |]
    where url   = getContentUrlFromNode
          title = getTitlePropertyOrEmpty

auditTrail :: [Text] -> Widget
auditTrail path = do app         <- getYesod
                     eitherNodes <- liftIO $ runEitherT $ do
                         let path'    = map T.unpack path
                         let repoPath = case path' of
                                            ("content" : rest)          -> rest
                                            ("edit" : "content" : rest) -> rest
                                            _                           -> fail "not a content node"
                         node        <- getNode (contentRepo app) repoPath
                         getTrail node
                     case eitherNodes of
                         Left _ ->
                             return ()
                         Right nodes -> mapM_ encodeTrail nodes
    where encodeTrail n = [whamlet|<li .menu-123 .collapsed>
                                       <a href=#{url n} title=#{title n}>#{title n}
                          |]
          url   = getContentUrlFromNode
          title = getTitlePropertyOrEmpty

galleryNavigation :: [Text] -> Widget
galleryNavigation path = do
  app             <- getYesod
  eitherGalleries <- liftIO $ runEitherT $ list_galleries $ galleryRepo app
  case eitherGalleries of
    Left ioe -> do
      $logError $ T.pack $ show (map T.unpack path) ++ "/galleryNavigation: " ++ show (ioe :: IOException)
      return ()
    Right galleries -> case path of
      ("galleries" : _)        -> mapM_ (renderGallery   Nothing)   galleries
      ("gallery"   : self : _) -> mapM_ (renderGallery $ Just self) galleries
      _                        -> return ()

  where
    renderGallery maybeSelf g = do
      let gname = T.pack $ gallery_name g
      case maybeSelf of
        Just self | self == gname ->
          [whamlet|
              <li .expanded>
                  <a href=@{GalleryR gname} title=#{gname}>#{gname}
          |]
        _ ->
          [whamlet|
              <li .collapsed>
                  <a href=@{GalleryR gname} title=#{gname}>#{gname}
          |]


getContentUrlFromNode :: Node -> String
getContentUrlFromNode = ("/content/" ++) . urlToString . node_path

getTitlePropertyOrEmpty :: Node -> String
getTitlePropertyOrEmpty = maybe "" show . fmap prop_value . flip getProperty ("title" :: String)

getTrail :: Node -> RepositoryContext [Node]
getTrail node = do nodes       <- tail <$> getParentNodes node -- tail: omit root node
                   if null nodes
                   then left $ userError "no root"
                   else do
                      welcome  <- getNode (node_repo node) (urlFromString "/welcome")
                      let first = node_path $ head nodes
                      case first of ["welcome"] -> return nodes
                                    _           -> return (welcome : nodes)

getParentNodes :: Node -> RepositoryContext [Node]
getParentNodes node = foldrM appendToNodes [node] $ node_path node
    where
          appendToNodes :: PathComponent -> [Node] -> RepositoryContext [Node]
          appendToNodes _ (n : ns) = do p <- getParentNode n
                                        return (p : n : ns)
          appendToNodes _ _ = undefined

------------------------------------------------------------------------------------------
--- Member Calendar
------------------------------------------------------------------------------------------

data Event = Event { evTitle       :: Text
                   , evStartDate   :: DateTime
                   , evEndDate     :: Maybe DateTime
                   , evDescription :: Maybe Text
                   , evLocation    :: Maybe Text
                   } deriving (Show)

instance Persistent Event where

    fromNode node = Event (T.pack $ node_name node)
                          (fromMaybe startOfTime $ dateProperty "startDate")
                          (dateProperty "endDate")
                          (textProperty "description")
                          (textProperty "location")
      where textProperty p = T.pack . show . prop_value <$> getProperty node p
            dateProperty = fromSqlString . show . maybe (StringValue "") prop_value . getProperty node

    toNode repo eventName event = Node eventName
                                         (urlFromString eventName)
                                         [ Property "startDate"   $ StringValue $          toSqlString        $ evStartDate   event
                                         , Property "endDate"     $ StringValue $ maybe "" toSqlString        $ evEndDate     event
                                         , Property "description" $ StringValue $ T.unpack     $ fromMaybe "" $ evDescription event
                                         , Property "location"    $ StringValue $ T.unpack     $ fromMaybe "" $ evLocation    event
                                         ]
                                         repo

instance ToJSON Event where
    toJSON Event {..} = object [ "title"       .=                               evTitle
                               , "startDate"   .=               toSqlString     evStartDate
                               , "endDate"     .= fromMaybe "" (toSqlString <$> evEndDate)
                               , "description" .= fromMaybe ""                  evDescription
                               , "location"    .= fromMaybe ""                  evLocation
                               ]

getMemberCalendarR :: Handler ()
getMemberCalendarR = do
    (year, month, _) <- liftIO $ liftM toGregorian' getCurrentTime
    redirect $ MemberCalendarMR (fromInteger year) month

getMemberCalendarMR :: Int -> Int -> Handler TypedContent
getMemberCalendarMR year month = if   month < 1 || month > 12
                                 then notFound
                                 else selectRep $ do
    provideRep $ renderEvents $ \events -> cbgLayout ["members", "calendar"]
        [whamlet|
            <table>
                <tr>
                    <th>Startdatum
                    <th>Enddatum
                    <th>Titel
                    <th>Ort
                    <th>Beschreibung
                $forall event <- events
                    <tr>
                        <td>#{               toSqlString  $  evStartDate   event}
                        <td>#{fromMaybe "" $ toSqlString <$> evEndDate     event}
                        <td>#{                               evTitle       event}
                        <td>#{fromMaybe "" $                 evLocation    event}
                        <td>#{fromMaybe "" $                 evDescription event}
                        <td>
                            <a href=@{EventR $ eventId event}>
                                <button>Edit
        |]
    provideRep $ renderEvents returnJson
  where renderEvents :: HasContentType a => ([Event] -> Handler a) -> Handler a
        renderEvents as = do
              app          <- getYesod
              eitherEvents <- liftIO $ runEitherT $ getNode (calendarRepo app) url >>= getEventList
              case eitherEvents of
                  Left  e      -> do $logError $ T.pack $ show e
                                     as ([] :: [Event])
                  Right events -> as events

        url = map (pathCompFromString . show) [year, month]

        getEventList :: Node -> RepositoryContext [Event]
        getEventList node = do
         nodes     <- getChildNodesRecursively node
         let nodes' = filter ((== 4) . length . node_path) nodes
         return $ map fromNode nodes'

        eventId event = T.pack $ intercalate "/" [show year', show month', toSqlString $ evStartDate event, T.unpack $ evTitle event]
            where (year', month', _) = toGregorian' $ evStartDate event

eventForm :: Maybe Event -> Html -> MForm Handler (FormResult Event, Widget)
eventForm mevent = renderDivs $ makeEvent
    <$> areq textField "Titel"        (                                fmap evTitle       mevent)
    <*> areq textField "Startdatum"   (     (T.pack . toSqlString) <$> fmap evStartDate   mevent)
    <*> aopt textField "Enddatum"     (fmap (T.pack . toSqlString) <$> fmap evEndDate     mevent)
    <*> aopt textField "Beschreibung" (                                fmap evDescription mevent)
    <*> aopt textField "Ort"          (                                fmap evLocation    mevent)
  where makeEvent :: Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Event
        makeEvent title start end = Event title
                                          (fromMaybe startOfTime $ fromSqlString $ T.unpack     start)
                                          (fromMaybe Nothing     $ fromSqlString . T.unpack <$> end)

getEventR :: Text -> Handler Html
getEventR name = do app               <- getYesod
                    eitherEvent       <- liftIO $ readItem (calendarRepo app) (T.unpack name)
                    let mevent        =  either (\e -> trace (show e) Nothing)
                                                Just
                                                eitherEvent
                    (widget, encType) <- generateFormPost $ eventForm mevent
                    cbgLayout ["members", "event"] [whamlet|
                       <form method=post action=@{EventR name} enctype=#{encType}>
                           ^{widget}
                           <button>Submit
                    |]

postEventR :: Text -> Handler Html
postEventR name = do
    ((result, widget), enctype) <- runFormPost $ eventForm Nothing
    case result of FormSuccess event -> do app               <- getYesod
                                           let strName       =  T.unpack name
                                           -- this should actually be removeItem (old path) >> writeItem (new path)
                                           result' <- liftIO $ writeItem (calendarRepo app) strName event
                                           case result' of
                                               Left e -> return $ trace (show e) ()
                                               _      -> return ()
                                           let (year, month, _) = toGregorian' $ evStartDate event
                                           redirect $ MemberCalendarMR (fromInteger year) month
                   _                 -> cbgLayout ["members", "event"] [whamlet|
                                            <p>Da stimmt etwas nicht, versuch's nochmal
                                            <form method=post action=@{EventR name} enctype=#{enctype}>
                                                ^{widget}
                                                <button>Submit
                                        |]


------------------------------------------------------------------------------------------
--- Member List
------------------------------------------------------------------------------------------

-- TODO: Template Haskell was probably invented for this kind of boilerplate stuff...

data Member = Member { firstname :: Text
                     , name      :: Text
                     , address   :: Maybe Text
                     , locality  :: Maybe Text
                     , status    :: Text
                     , phone     :: Maybe Text
                     , mobile    :: Maybe Text
                     , email     :: Maybe Text
                     } deriving (Show, Eq)

instance Ord Member where
  compare a b = compare (key a) (key b)
    where
      key m = show (name m) ++ show (firstname m)

instance ToJSON Member where
    toJSON Member {..} = object ["firstname"  .= firstname
                                ,"name"       .= name
                                ,"address"    .= address
                                ,"locality"   .= locality
                                ,"status"     .= status
                                ,"phone"      .= phone
                                ,"mobile"     .= mobile
                                ,"email"      .= email
                                ]

instance Persistent Member where
    fromNode node = Member (req_property "firstname")
                           (req_property "name")
                           (opt_property "address")
                           (opt_property "locality")
                           (req_property "status")
                           (opt_property "phone")
                           (opt_property "mobile")
                           (opt_property "email")
      where req_property = T.pack . show . fromMaybe (StringValue "") . liftM prop_value . getProperty node
            opt_property pname = (T.pack . show) <$> liftM prop_value (getProperty node pname)
    toNode repo memberName member = Node memberName
                                         (urlFromString memberName)
                                         [ Property "firstname" $ StringValue $ T.unpack $                firstname member
                                         , Property "name"      $ StringValue $ T.unpack $                name      member
                                         , Property "address"   $ StringValue $ T.unpack $ fromMaybe "" $ address   member
                                         , Property "locality"  $ StringValue $ T.unpack $ fromMaybe "" $ locality  member
                                         , Property "status"    $ StringValue $ T.unpack $                status    member
                                         , Property "phone"     $ StringValue $ T.unpack $ fromMaybe "" $ phone     member
                                         , Property "mobile"    $ StringValue $ T.unpack $ fromMaybe "" $ mobile    member
                                         , Property "email"     $ StringValue $ T.unpack $ fromMaybe "" $ email     member
                                         ]
                                         repo

memberForm :: Maybe Member -> Html -> MForm Handler (FormResult Member, Widget)
memberForm mmember = renderDivs $ Member
    <$> areq textField "Vorname" (firstname <$> mmember)
    <*> areq textField "Name"    (name      <$> mmember)
    <*> aopt textField "Strasse" (address   <$> mmember)
    <*> aopt textField "Ort"     (locality  <$> mmember)
    <*> areq textField "Status"  (status    <$> mmember)
    <*> aopt textField "Telefon" (phone     <$> mmember)
    <*> aopt textField "Mobile"  (mobile    <$> mmember)
    <*> aopt textField "E-Mail"  (email     <$> mmember)

getMemberR :: Text -> Handler Html
getMemberR name = do app               <- getYesod
                     eitherMember      <- liftIO $ readItem (memberRepo app) (T.unpack name)
                     let mmember       =  either (\e -> trace (show e) Nothing)
                                                 Just
                                                 eitherMember
                     (widget, encType) <- generateFormPost $ memberForm mmember
                     cbgLayout ["members", "list", "edit"] [whamlet|
                        <form method=post action=@{MemberR name} enctype=#{encType}>
                            ^{widget}
                            <button>Submit
                     |]

postMemberR :: Text -> Handler Html
postMemberR name = do
    ((result, widget), enctype) <- runFormPost $ memberForm Nothing
    case result of FormSuccess member -> do app               <- getYesod
                                            result' <- liftIO $ writeItem (memberRepo app) (T.unpack name) member
                                            case result' of
                                                Left e -> return $ trace (show e) ()
                                                _      -> return ()
                                            redirect MemberListR
                   _                  -> cbgLayout ["members", "list", "edit"] [whamlet|
                                             <p>Da stimmt etwas nicht, versuch's nochmal
                                             <form method=post action=@{MemberR name} enctype=#{enctype}>
                                                 ^{widget}
                                                 <button>Submit
                                         |]

getMemberListR :: Handler TypedContent
getMemberListR = selectRep $ do
    provideRep $ renderMembers $ \members ->
                cbgLayout ["members", "list"] [whamlet|
                    <table>
                        <tr>
                            <th>Vorname
                            <th>Name
                            <th>Strasse
                            <th>Ort
                            <th>Status
                            <th>Telefon
                            <th>Mobile
                            <th>E-Mail
                        $forall member <- members
                            <tr>
                                <td>#{               firstname member}
                                <td>#{               name      member}
                                <td>#{fromMaybe "" $ address   member}
                                <td>#{fromMaybe "" $ locality  member}
                                <td>#{               status    member}
                                <td>#{fromMaybe "" $ phone     member}
                                <td>#{fromMaybe "" $ mobile    member}
                                <td>#{fromMaybe "" $ email     member}
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
                  Left  e       -> do $logError $ T.pack $ show e
                                      as ([] :: [Member])
                  Right members -> as members
        memberId member = T.concat [name member, T.pack " ", firstname member]
        getMemberList :: Node -> RepositoryContext [Member]
        getMemberList node = liftM (sort . map fromNode) (getChildNodes node)


------------------------------------------------------------------------------------------
--- Galleries
------------------------------------------------------------------------------------------

getGalleriesR :: Handler TypedContent
getGalleriesR = selectRep $
    provideRep $ do
        app             <- getYesod
        eitherGalleries <- liftIO $ runEitherT $ list_galleries $ galleryRepo app
        case eitherGalleries of
            Left  e         -> do $logError $ T.pack $ show e
                                  fail "Internal error while trying to list galleries."
            Right galleries ->
                cbgLayout ["galleries"] [whamlet|
                    <table>
                        <tr>
                            <th>Gallery Name
                        $forall gallery <- galleries
                            <tr>
                                <td>
                                    <a href=@{GalleryR (T.pack $ gallery_name gallery)}>#{gallery_name gallery}
                |]

getGalleryR :: Text -> Handler TypedContent
getGalleryR gname = selectRep $
    provideRep $ do
        app           <- getYesod
        eitherGallery <- liftIO $ runEitherT $ gallery_read (galleryRepo app) (T.unpack gname)
        case eitherGallery of
            Left  e         -> do $logError $ T.pack $ show e
                                  fail "Internal error while trying to list galleries."
            Right gallery ->
                cbgLayout ["gallery", gname] $
                    [whamlet|
                        <h1>#{gname}
                        <div .row>
                            $forall iname <- map T.pack $ gallery_images gallery
                                <div .galleryimg>
                                    <a href=@{GalleryImageR gname iname}>
                                        <img src=@{ImageR gname iname} alt=#{iname} width=171>
                            <div .galleryimg>
                                <input #fileinput type=file multiple=multiple accept="image/*">
                        <script>
                            function uploadFile(file) {
                                var xhr = new XMLHttpRequest();
                                var fd  = new FormData();
                                xhr.open('POST', '@{UploadImageR gname}', true);
                                xhr.onreadystatechange = function() {
                                    if (xhr.readyState == 4 && xhr.status == 200) {
                                        console.log("upload successful: " + xhr.responseText);
                                    }
                                };
                                fd.append('upload_file', file);
                                xhr.send(fd);
                            }
                            document.querySelector('#fileinput').addEventListener('change', function() {
                                for (var i = 0; i < this.files.length; ++i) {
                                    uploadFile(this.files[i]);
                                }
                            });
                    |]

postGalleryR :: Text -> Handler Html
postGalleryR _ = undefined

deleteGalleryR :: Text -> Handler Html
deleteGalleryR _ = undefined

getGalleryImagesR :: Text -> Handler TypedContent
getGalleryImagesR gname = selectRep $
    provideRep $ do
        app           <- getYesod
        eitherGallery <- liftIO $ runEitherT $ gallery_read (galleryRepo app) (T.unpack gname)
        case eitherGallery of
            Left  e         -> do $logError $ T.pack $ show e
                                  fail "Internal error while trying to list galleries."
            Right gallery ->
                cbgLayout ["gallery", gname] [whamlet|
                    <h1>#{gname}
                    <table>
                        <tr>
                            <th>image
                        $forall iname <- map T.pack $ gallery_images gallery
                            <tr>
                                <td>
                                    <a href=@{GalleryImageR gname iname}>#{iname}
                |]


getGalleryImageR :: Text -> Text -> Handler TypedContent
getGalleryImageR gname iname = selectRep $
    provideRep $ do
        app           <- getYesod
        eitherGallery <- liftIO $ runEitherT $ gallery_read (galleryRepo app) (T.unpack gname)
        case eitherGallery of
            Left  e       -> do
                $logError $ T.pack $ show e
                fail "Internal error while trying to list galleries."
            Right gallery -> do
                let images     = gallery_images gallery
                    Just imgno = elemIndex (T.unpack iname) images
                    imgprev    = if imgno < 1
                                 then []
                                 else take 1 $ drop (imgno - 1) images
                    imgnext    = take 1 $ drop (imgno + 1) images
                cbgLayout ["gallery", gname, iname] [whamlet|
                    <h1>#{gname} - #{iname}
                    <table>
                        <tr>
                            <th>
                                $case imgprev
                                    $of [prev]
                                        <a href=@{GalleryImageR gname (T.pack prev)}>&lt;- Vorheriges
                                    $of _
                                |
                                $case imgnext
                                    $of [next]
                                        <a href=@{GalleryImageR gname (T.pack next)}>Nächstes -&gt;
                                    $of _
                        <tr>
                            <td>
                                <a href=@{ImageR gname iname}>
                                    <img src=@{ImageR gname iname}>
                |]


postGalleryImageR :: Text -> Text -> Handler Html
postGalleryImageR _ _ = undefined

deleteGalleryImageR :: Text -> Text -> Handler Html
deleteGalleryImageR _ _ = undefined

getImageR :: Text -> Text -> Handler ()
getImageR gname iname = do
    app         <- getYesod
    eitherImage <- liftIO $ runEitherT $ do
        image <- image_read (galleryRepo app) (T.unpack gname) (T.unpack iname)
        blob  <- image_blob image
        return (image, blob)
    case eitherImage of
        Left e -> do
            $logError $ T.pack $ show e
            fail "Internal error while trying to load image."
        Right (image, blob) ->
            sendWaiResponse $ responseLBS status200 [("Content-Type",  U8.fromString $ image_type image)] blob

postUploadImageR :: Text -> Handler Html
postUploadImageR gname = do
    (_, files) <- runRequestBody
    mauthUser <- maybeAuthId
    case mauthUser of
        Nothing -> permissionDenied ""
        Just userName -> do
            let upload (_, file) = do
                  app <- getYesod
                  let iname = fileName file
                  let type' = fileContentType file
                  bytes <- runConduit $ fileSource file $$ CB.sinkLbs
                  eitherResult <- liftIO $
                      runEitherT $ image_write (galleryRepo app) (T.unpack gname) (T.unpack iname) (T.unpack type') (T.unpack userName) bytes
                  case eitherResult of
                      Left e -> do
                          $logError $ T.pack $ show e
                          fail "Internal error while trying to save image."
                      Right _ -> return ()
            mapM_ upload files
            cbgLayout ["upload", "image"] [whamlet|
                <h1>File upload status
            |]


------------------------------------------------------------------------------------------
--- Assets
------------------------------------------------------------------------------------------

getAssetR :: ContentPath -> Handler ()
getAssetR (ContentPath path) = do
  repo <- liftM assetRepo getYesod
  let filePath = intercalate "/" $ map T.unpack path
  eitherAsset <- liftIO $ runEitherT $ assetRead repo filePath
  case eitherAsset of
    Left e -> do
      $logError $ T.pack $ show e
      notFound
    Right asset -> sendFile (U8.fromString $ assetType asset) $ trace (show $ assetBlob asset) (assetBlob asset)
