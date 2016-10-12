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
import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Calendar
import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Event
import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Gallery
import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Member
import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Navigation
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.Privileges
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.Users

-- Yesod
import           Network.HTTP.Conduit                               (Manager)
import           Text.Hamlet
import           Yesod                                              hiding
                                                                     (deleteBy,
                                                                     joinPath,
                                                                     renderBootstrap)
import           Yesod.Auth
import           Yesod.Auth.Account
import           Yesod.Auth.GoogleEmail2
import           Yesod.Static

-- other imports
import           Control.Concurrent                                 (MVar)
import           Control.Exception                                  (IOException)
import           Control.Exception.Base                             (throwIO)
import           Control.Monad                                      (liftM)
import           Control.Monad.Trans.Either                         (left,
                                                                     runEitherT)
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Lazy.UTF8                          as UL8
import qualified Data.ByteString.UTF8                               as U8
import           Data.Conduit
import qualified Data.Conduit.Binary                                as CB
import           Data.DateTime
import           Data.Foldable                                      (foldrM)
import           Data.Function                                      (on)
import           Data.List                                          (deleteBy,
                                                                     elemIndex,
                                                                     intercalate,
                                                                     isPrefixOf,
                                                                     partition,
                                                                     sort,
                                                                     sortBy)
import           Data.Maybe                                         (fromMaybe)
import           Data.Text                                          (Text)
import qualified Data.Text                                          as T
import qualified Data.Text.Lazy                                     as TL
import qualified Data.Tree                                          as Tree
import           Database.Persist.Sqlite                            (ConnectionPool,
                                                                     SqlBackend,
                                                                     runSqlPool)
import           Debug.Trace
import           Network.Mail.Mime
import           System.FilePath                                    ((</>))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
User
    username Text
    UniqueUsername username
    password B.ByteString
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
                             , assetRepo'   :: Repository
                             , clientId     :: Text
                             , clientSecret :: Text
                             , dbPool       :: ConnectionPool
                             }

mkYesod "CBGWebSite" [parseRoutes|
    /                                 RootR                 GET
    /favicon.ico                      FavR                  GET
    /static                           StaticR               Static          getStatic
    /auth                             AuthR                 Auth            getAuth
    /members                          MembersR              GET
    /content/+ContentPath             ContentR              GET
    /edit/content/+ContentPath        EditContentR          GET POST
    /members/calendar                 MemberCalendarR       GET
    /members/calendar/month/#Int/#Int MemberCalendarMR      GET
    /members/event/edit/#Text         EventR                GET POST
    /members/list                     MemberListR           GET
    /members/list/edit/#Text          MemberR               GET POST
    /galleries                        GalleriesR            GET
    /gallery/#Text                    GalleryR              GET POST DELETE
    /gallery/#Text/images             GalleryImagesR        GET
    /gallery/#Text/image/#Text        GalleryImageR         GET POST DELETE
    /image/orig/#Text/#Text           ImageR                GET
    /image/small/#Text/#Text          ImageSmallR           GET
    /image/thumb/#Text/#Text          ImageThumbR           GET
    /upload/image/#Text               UploadImageR          POST
    /assets/+ContentPath              AssetsR               GET
    /asset/+ContentPath               AssetR                GET POST PUT
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

    isAuthorized (ImageSmallR _ _)      False = isAuthorized MembersR False

    isAuthorized (ImageThumbR _ _)      False = isAuthorized MembersR False

    isAuthorized (UploadImageR _)       True  = isAuthorized MembersR False

    -- the assets
    isAuthorized (AssetR _)             False = isAuthorized MembersR False
    isAuthorized (AssetsR _)            False = isAuthorized MembersR False
    isAuthorized (AssetR _)             True  = isAuthorized MembersR False

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
editLayout path = cbgLayout ("edit" : path) . editPage

getRootR :: Handler ()
getRootR = redirect ("/content" :: String)

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
    case eitherNode of
        Left _ -> notFound
        Right node -> do
          res <- liftIO $ runEitherT $ liftM UL8.toString $ getProperty node "text.html"
          prop <- either (fail . show) return res
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
        Right node -> do
          body <- runInputPost $ ireq textField "body"
          liftIO $ runEitherT $ writeProperty node "text.html" (UL8.fromString $ T.unpack body)
          --redirect $ joinPath ("/content/" : (map unpack pieces))
          withUrlRenderer [hamlet||]

getContentR :: ContentPath -> Handler Html
getContentR (ContentPath pieces) = cbgLayout ("content" : pieces) $ do
    app <- getYesod
    eitherNode <- liftIO $ runEitherT $ do
        let url = map T.unpack pieces
        getNode (contentRepo app) url
    case eitherNode of
        Left _ -> notFound
        Right node -> do
          res <- liftIO $ runEitherT $ liftM UL8.toString $ getProperty node "text.html"
          prop <- either (fail.show) return res
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
                                             ^{contentNavigationFromRoot}
                                             <ul .menu>
                                               <li .collapsed>
                                                 <a href=@{MembersR} title="Mitglieder">Mitglieder
                                   |]
                               ("members" : _) ->
                                   [whamlet|
                                             <ul .menu>
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
                                             <ul .menu>
                                               <li .collapsed>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                               <li .expanded>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                                   <ul .menu>
                                                       <li .expanded>
                                                           <a href=@{GalleriesR} title=Fotoalben>Fotoalben
                                                           <ul .menu>
                                                               ^{galleryNavigation path}
                                   |]
                               ("gallery" : _) ->
                                   [whamlet|
                                             <ul .menu>
                                               <li .collapsed>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                               <li .expanded>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                                   <ul .menu>
                                                       <li .expanded>
                                                           <a href=@{GalleriesR} title=Fotoalben>Fotoalben
                                                           <ul .menu>
                                                               ^{galleryNavigation path}
                                   |]
                               _ ->
                                   [whamlet|
                                             <ul .menu>
                                               <li .collapsed>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                               <li .collapsed>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                   |]
                       Nothing ->
                               [whamlet|
                                               ^{contentNavigationFromRoot}
                                               <ul .menu>
                                                   <li .leaf .last>
                                                       <a href=@{AuthR LoginR} title="Mitglieder">Mitglieder
                               |]
    [whamlet|
        <div #block-menu-primary-links .clear-block .block .block-menu>
            <h1>Come Back Glöbb
            <div .content>
                ^{casePart}
    |]

contentNavigationFromRoot :: Widget
contentNavigationFromRoot = contentNavigation ["content"]

contentNavigation :: [Text] -> Widget
contentNavigation path = do app         <- getYesod
                            let path'    =  map T.unpack path
                            let repoPath =  case path' of
                                                ("content" : rest)          -> rest
                                                ("edit" : "content" : rest) -> rest
                                                _                           -> fail "no content node"
                            eitherNodes <- liftIO $ runEitherT $ getNavigation (contentRepo app) (urlFromStrings repoPath) 2
                            case eitherNodes of
                                Left _ ->
                                  return ()
                                Right navi ->
                                  renderNavi navi

renderNavi :: Navigation NavigationEntry -> Widget
renderNavi Navigation {..} = do
  let self     = Tree.rootLabel navTree
      siblings = sort (self : navSiblings)
      children = Tree.subForest navTree
      url      = getContentUrlFromURL . neURL
  [whamlet|
    <ul .menu>
      $forall entry <- siblings
        $if entry == self
          <li .collapsed>
            <a href=#{url self} title=#{neTitle self}>#{neTitle self}
            $if (not . null) children
              $forall child <- children
                ^{renderTree child}
        $else
          <li .collapsed>
            <a href=#{url entry} title=#{neTitle entry}>#{neTitle entry}
  |]

renderTree :: Tree.Tree NavigationEntry -> Widget
renderTree tree = do
  let url  = getContentUrlFromURL . neURL
      self = Tree.rootLabel tree
      children = Tree.subForest tree
  [whamlet|
    <ul .menu>
      <li .expanded>
        <a href=#{url self} title=#{neTitle self}>#{neTitle self}
        $if (not . null) children
          $forall child <- children
            ^{renderTree child}
  |]

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
                                       <a href=#{url n} title=#{neTitle n}>#{neTitle n}
                          |]
          url   = getContentUrlFromURL . neURL

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


getContentUrlFromURL :: URL -> String
getContentUrlFromURL = ("/content/" ++) . urlToString


------------------------------------------------------------------------------------------
--- Member Calendar
------------------------------------------------------------------------------------------

getMemberCalendarR :: Handler ()
getMemberCalendarR = do
    (year, month, _) <- liftIO $ liftM toGregorian' getCurrentTime
    redirect $ MemberCalendarMR (fromInteger year) month

getMemberCalendarMR :: Int -> Int -> Handler TypedContent
getMemberCalendarMR year month = if   month < 1 || month > 12
                                 then notFound
                                 else selectRep $ do

    provideRep $ do events              <- sortBy (compare `on` evStartDate) <$> getEvents
                    let cal              = calendarForMonth year month         :: [[DateTime]]
                        calWithoutEvents = map (map (\d -> (d, []))) cal       :: [[(DateTime, [Event])]]
                        -- mapping with the full list of events is not very efficient, it would be
                        -- better to remove the events from the list that have already been considered
                        calWithEvents = map (mergeCal events) calWithoutEvents :: [[(DateTime, [Event])]]
                    cbgLayout ["members", "calendar"]
                        [whamlet|
                            <div .container>
                                <div .row>
                                    <div .col-md-8 .col-md-offset-1>
                                        <div #calendar .container>
                                            $forall week <- calWithEvents
                                                <div .row>
                                                    $forall day <- week
                                                        <div .col-md-1>
                                                            <div .thumbnail>#{dayno $ fst day}
                                                                $forall event <- snd day
                                                                    <p>#{evTitle event}
                        |]

    provideRep $ renderEvents returnJson

  where renderEvents :: HasContentType a => ([Event] -> Handler a) -> Handler a
        renderEvents as = do
              app          <- getYesod
              eitherEvents <- liftIO $ runEitherT $ getEventsForMonth (calendarRepo app) year month
              case eitherEvents of
                  Left  e      -> do $logError $ T.pack $ show e
                                     as ([] :: [Event])
                  Right events -> as events

        getEvents :: Handler [Event]
        getEvents = do
              app          <- getYesod
              eitherEvents <- liftIO $ runEitherT $ getEventsForMonth (calendarRepo app) year month
              case eitherEvents of
                  Left  e      -> do $logError $ T.pack $ show e
                                     return ([] :: [Event])
                  Right events -> return events

        eventId event = T.pack $ intercalate "/" [show year', show month', toSqlString $ evStartDate event, T.unpack $ evTitle event]
            where (year', month', _) = toGregorian' $ evStartDate event

        dayno :: DateTime -> Int
        dayno d = let (_, _, dno) = toGregorian' d in dno

        mergeCal :: [Event] -> [(DateTime, [Event])] -> [(DateTime, [Event])]
        mergeCal _ [] = []
        mergeCal events ((cal, _) : calRest) = (cal, eventsAt) : mergeCal eventsAfter calRest
          where
            (eventsAt, eventsAfter) = partition (\ev -> toGregorian' cal == toGregorian' (evStartDate ev)) events

eventForm :: CBGWebSite -> Maybe Event -> Html -> MForm Handler (FormResult Event, Widget)
eventForm app mevent = do
  renderDivs $ makeEvent (contentRepo app)
    <$> areq textField "Titel"        (                                fmap evTitle       mevent)
    <*> areq textField "Startdatum"   (     (T.pack . toSqlString) <$> fmap evStartDate   mevent)
    <*> aopt textField "Enddatum"     (fmap (T.pack . toSqlString) <$> fmap evEndDate     mevent)
    <*> areq textField "Beschreibung" (                                fmap evDescription mevent)
    <*> areq textField "Ort"          (                                fmap evLocation    mevent)
  where makeEvent :: Repository -> Text -> Text -> Maybe Text -> Text -> Text -> Event
        makeEvent repo title start end = Event repo
                                               title
                                               (fromMaybe startOfTime $ fromSqlString $ T.unpack     start)
                                               (fromMaybe Nothing     $ fromSqlString . T.unpack <$> end)

getEventR :: Text -> Handler Html
getEventR name = do app               <- getYesod
                    eitherEvent       <- liftIO $ runEitherT $ readItem (calendarRepo app) (T.unpack name)
                    let mevent        =  either (\e -> trace (show e) Nothing)
                                                Just
                                                eitherEvent
                    (widget, encType) <- generateFormPost $ eventForm app mevent
                    cbgLayout ["members", "event"] [whamlet|
                       <form method=post action=@{EventR name} enctype=#{encType}>
                           ^{widget}
                           <button>Submit
                    |]

postEventR :: Text -> Handler Html
postEventR name = do
  app <- getYesod
  ((result, widget), enctype) <- runFormPost $ eventForm app Nothing
  case result of FormSuccess event -> do app               <- getYesod
                                         let strName       =  T.unpack name
                                         -- this should actually be removeItem (old path) >> writeItem (new path)
                                         result' <- liftIO $ runEitherT $ writeItem event
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

renderBootstrap aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
        widget = [whamlet|
                        \#{fragment}
                        $forall view <- views
                            <div .form-group :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
                                <label .col-md-2 .control-label for=#{fvId view}>#{fvLabel view}
                                <div .col-md-10>
                                    ^{fvInput view}
                 |]
    return (res, widget)

memberForm :: CBGWebSite -> Maybe Member -> Html -> MForm Handler (FormResult Member, Widget)
memberForm app mmember = do
  let repo = memberRepo app
  renderBootstrap $ Member repo
    <$> areq textField (fieldDef "Vorname") (memFirstname <$> mmember)
    <*> areq textField (fieldDef "Name")    (memName      <$> mmember)
    <*> areq textField (fieldDef "Strasse") (memAddress   <$> mmember)
    <*> areq textField (fieldDef "Ort")     (memLocality  <$> mmember)
    <*> areq textField (fieldDef "Status")  (memStatus    <$> mmember)
    <*> areq textField (fieldDef "Telefon") (memPhone     <$> mmember)
    <*> areq textField (fieldDef "Mobile")  (memMobile    <$> mmember)
    <*> areq textField (fieldDef "E-Mail")  (memEmail     <$> mmember)
  where fieldDef name = name { fsAttrs = [("class", "form-control")] }

getMemberR :: Text -> Handler Html
getMemberR name = do app          <- getYesod
                     eitherMember <- liftIO $ runEitherT $ readItem (memberRepo app) (T.unpack name)
                     let mmember  =  either (\e -> trace (show e) Nothing)
                                            Just
                                            eitherMember
                     (widget, encType) <- generateFormPost $ memberForm app mmember
                     cbgLayout ["members", "list", "edit"] [whamlet|
                             <form .form-horizontal method=post action=@{MemberR name} enctype=#{encType}>
                                 ^{widget}
                                 <div .form-group>
                                     <button>Submit
                     |]

postMemberR :: Text -> Handler Html
postMemberR name = do
  app <- getYesod
  ((result, widget), enctype) <- runFormPost $ memberForm app Nothing
  case result of FormSuccess member -> do result' <- liftIO $ runEitherT $ writeItem member
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
                                <td>#{memFirstname member}
                                <td>#{memName      member}
                                <td>#{memAddress   member}
                                <td>#{memLocality  member}
                                <td>#{memStatus    member}
                                <td>#{memPhone     member}
                                <td>#{memMobile    member}
                                <td>#{memEmail     member}
                                <td>
                                    <a href=@{MemberR $ memberId member}>
                                        <button>Edit
                |]
    provideRep $ renderMembers returnJson
  where renderMembers :: HasContentType a => ([Member] -> Handler a) -> Handler a
        renderMembers as = do
              app           <- getYesod
              eitherMembers <- liftIO $ runEitherT $ getMemberList (memberRepo app)
              case eitherMembers of
                  Left  e       -> do $logError $ T.pack $ show e
                                      as ([] :: [Member])
                  Right members -> as members


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
                                        <img src=@{ImageThumbR gname iname} alt=#{iname} width=171>
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
                        <div .row>
                            <h1>#{gname} - #{iname}
                        <div .row>
                                    $case imgprev
                                        $of [prev]
                                            <a .col-md-1 href=@{GalleryImageR gname (T.pack prev)}>
                                                <button .btn .btn-primary>&lt;- Vorheriges
                                        $of _
                                            <div .col-md-1>&nbsp;
                                    |
                                    $case imgnext
                                        $of [next]
                                            <a .col-md-1 .col-md-offset-9 href=@{GalleryImageR gname (T.pack next)}>
                                                <button .btn .btn-primary>Nächstes -&gt;
                                        $of _
                        <div .row style="margin-top: 20px;">
                                    <a href=@{ImageR gname iname}>
                                        <img .galleryimg-small src=@{ImageSmallR gname iname}>
                |]


postGalleryImageR :: Text -> Text -> Handler Html
postGalleryImageR _ _ = undefined

deleteGalleryImageR :: Text -> Text -> Handler Html
deleteGalleryImageR _ _ = undefined

getImageR :: Text -> Text -> Handler ()
getImageR gname iname = do
    app         <- getYesod
    eitherImage <- liftIO $ runEitherT $ image_read (galleryRepo app) (T.unpack gname) (T.unpack iname)
    case eitherImage of
        Left e -> do
            $logError $ T.pack $ show e
            notFound
        Right image -> sendFile (U8.fromString $ image_type image) (image_blob image)

getImageSmallR :: Text -> Text -> Handler ()
getImageSmallR gname iname = do
    app         <- getYesod
    eitherImage <- liftIO $ runEitherT $ image_read (galleryRepo app) (T.unpack gname) (T.unpack iname)
    case eitherImage of
        Left e -> do
            $logError $ T.pack $ show e
            notFound
        Right image -> sendFile (U8.fromString $ image_type image) (image_small image)

getImageThumbR :: Text -> Text -> Handler ()
getImageThumbR gname iname = do
    app         <- getYesod
    eitherImage <- liftIO $ runEitherT $ image_read (galleryRepo app) (T.unpack gname) (T.unpack iname)
    case eitherImage of
        Left e -> do
            $logError $ T.pack $ show e
            notFound
        Right image -> sendFile (U8.fromString $ image_type image) (image_thumb image)

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
  repo <- liftM assetRepo' getYesod
  let filePath = intercalate "/" $ map T.unpack path
  eitherAsset <- liftIO $ runEitherT $ assetRead repo filePath
  case eitherAsset of
    Left e -> do
      $logError $ T.pack $ show e
      notFound
    Right asset -> sendFile (U8.fromString $ assetType asset) $ trace (show $ assetBlob asset) (assetBlob asset)

postAssetR :: ContentPath -> Handler ()
postAssetR (ContentPath path) = do
  (_, files) <- runRequestBody
  mauthUser  <- maybeAuthId
  case mauthUser of
    Nothing -> permissionDenied ""
    Just userName -> do
      let upload (_, file) = do
           app          <- getYesod
           let name      = fileName file
               type'     = fileContentType file
           bytes        <- runConduit $ fileSource file $$ CB.sinkLbs
           eitherResult <- liftIO $ runEitherT $ do
             now <- liftIO getCurrentTime
             assetWrite (assetRepo' app) (map T.unpack path) (T.unpack name) (T.unpack type') (T.unpack userName) now bytes
           case eitherResult of
             Left e -> do
               $logError $ T.pack $ show e
               fail "Internal error while trying to save asset."
             Right _ -> return ()
      mapM_ upload files

putAssetR :: ContentPath -> Handler ()
putAssetR (ContentPath path) = do
  mauthUser <- maybeAuthId
  app   <- getYesod
  let name = last path
      type' = "unknown" -- TODO: extract content type header
      userName = fromMaybe "anonymous" mauthUser
  bytes <- runConduit $ rawRequestBody $$ CB.sinkLbs
  eitherResult <- liftIO $ runEitherT $ do
    now <- liftIO getCurrentTime
    assetWrite (assetRepo' app) (map T.unpack path) (T.unpack name) (T.unpack type') (T.unpack userName) now bytes
  case eitherResult of
    Left e -> do
      $logError $ T.pack $ show e
      fail "Internal error while trying to save asset."
    Right _ -> return ()

getAssetsR :: ContentPath -> Handler Html
getAssetsR (ContentPath path) = do
  app <- getYesod
  assets' <- liftIO $ runEitherT $ listAssets (assetRepo' app) (map T.unpack path)
  case assets' of
    Left e -> do
      $logError $ T.pack $ show e
      fail "Internal error while trying to load asset."
    Right assets -> cbgLayout path [whamlet|
      <div .assets .row>
      $forall asset <- assets
        <div .asset .col-md-1>
          <a href=@{AssetsR $ ContentPath $ map T.pack (assetPath asset)} .thumbnail>
            $if isPrefixOf "image/" $ assetType asset
              <img src=#{assetBlob asset}>
            $else
              <span .glyphicon .glyphicon-folder-open>
    |]
