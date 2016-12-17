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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Assets where

-- CBG
import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Asset
import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Navigation
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.Layout
import           CH.ComeBackGloebb.CBGWebSite.Web.Component

-- Yesod
import Yesod
import Yesod.Auth
import Text.Hamlet

-- other imports
import           Control.Monad                                      (liftM)
import           Control.Monad.Trans.Either                         (runEitherT)
import qualified Data.ByteString.UTF8                               as U8
import           Data.Conduit
import qualified Data.Conduit.Binary                                as CB
import           Data.DateTime
import           Data.List (intercalate, isPrefixOf, find)
import           Data.Maybe                                         (fromMaybe)
import qualified Data.Text as T
import qualified Data.Tree as Tree

component :: Handler (Component CBGWebSite)
component = do
  cs <- components <$> getYesod
  let mc = find (== "Assets") cs
  case mc of
    Nothing -> fail "Component not found."
    Just c  -> return c

component' :: WidgetT CBGWebSite IO (Component CBGWebSite)
component' = do
  cs <- components <$> getYesod
  let mc = find (== "Assets") cs
  case mc of
    Nothing -> fail "Component not found."
    Just c  -> return c

getAssetR :: ContentPath -> Handler ()
getAssetR (ContentPath path) = do
  repo <- compRepository <$> component
  let filePath = intercalate "/" $ map T.unpack path
  eitherAsset <- liftIO $ runEitherT $ assetRead repo filePath
  case eitherAsset of
    Left e -> do
      $logError $ T.pack $ show e
      notFound
    Right asset -> sendFile (U8.fromString $ assetType asset) (assetBlob asset)

postAssetR :: ContentPath -> Handler ()
postAssetR (ContentPath path) = do
  (params, files) <- runRequestBody
  mauthUser  <- maybeAuthId
  case mauthUser of
    Nothing -> permissionDenied ""
    Just userName -> do
      let upload (_, file) = do
           repo         <- compRepository <$> component
           let name      = fromMaybe "noName" $ lookup "fileName" params
               type'     = fileContentType file
           bytes        <- runConduit $ fileSource file $$ CB.sinkLbs
           eitherResult <- liftIO $ runEitherT $ do
             now <- liftIO getCurrentTime
             assetWrite repo (map T.unpack (path ++ [name])) (T.unpack name) (T.unpack type') (T.unpack userName) now (Just bytes)
           case eitherResult of
             Left e -> do
               $logError $ T.pack $ show e
               fail "Internal error while trying to save asset."
             Right _ -> return ()
      mapM_ upload files
      redirect $ AssetsR $ ContentPath path

putAssetR :: ContentPath -> Handler ()
putAssetR (ContentPath path) = do
  mauthUser <- maybeAuthId
  repo      <- compRepository <$> component
  let name = last path
      type' = "application/x-directory"
      userName = fromMaybe "anonymous" mauthUser
  eitherResult <- liftIO $ runEitherT $ do
    now <- liftIO getCurrentTime
    assetWrite repo (map T.unpack path) (T.unpack name) (T.unpack type') (T.unpack userName) now Nothing
  case eitherResult of
    Left e -> do
      $logError $ T.pack $ show e
      fail "Internal error while trying to save asset."
    Right _ -> return ()

deleteAssetR :: ContentPath -> Handler ()
deleteAssetR (ContentPath path) = do
  repo <- compRepository <$> component
  eitherResult <- liftIO $ runEitherT $ do
    assetDelete repo (map T.unpack path)
  case eitherResult of
    Left e -> do
      $logError $ T.pack $ show e
      fail "Internal error while trying to delete asset."
    Right _ -> return ()

getAssetsR :: ContentPath -> Handler Html
getAssetsR (ContentPath path) = do
  let parentPath =
        case path of
          [] -> []
          _  -> init path
  comp <- component
  let repo = compRepository comp
  assets' <- liftIO $ runEitherT $ do
    let path'  = map T.unpack path
    thisAsset <- assetRead repo $ urlToString $ map T.unpack path
    assets    <- listAssets repo path'
    return (thisAsset, assets)

  case assets' of
    Left e -> do
      $logError $ T.pack $ show e
      fail "Internal error while trying to load asset."
    Right (thisAsset, assets) -> layout comp ("assets" : path) [whamlet|
      <div .assets .row>
        <div #addFolder .modal .fade>
          <script>
            function addFolder(name) {
              \$.ajax(
                { url: "@{AssetR $ ContentPath path}/" + name
                , type: "PUT"
                , success: function(result) {
                    window.location.assign("@{AssetsR $ ContentPath path}/" + name);
                  }
                }
              );
            }
          <div .modal-dialog>
            <div .modal-content>
              <div .modal-header>
                <button type=button .close data-dismiss=modal aria-hidden=true>&times;
                <h4 .modal-title>Neuer Ordner
              <div .modal-body>
                <label for=folderName>Name des neuen Ordners
                <input #folderName type=text name=folderName>
              <div .modal-footer>
                <button type=button .btn .btn-default data-dismiss=modal>Schliessen
                <button type=submit .btn .btn-primary onClick=addFolder($('#folderName').val())>Erstellen
        <div #uploadFile .modal .fade>
          <div .modal-dialog>
            <div .modal-content>
              <form method=post action=@{AssetR $ ContentPath path} enctype="multipart/form-data">
                <div .modal-header>
                  <button type=button .close data-dismiss=modal aria-hidden=true>&times;
                  <h4 .modal-title>Datei hochladen
                <div .modal-body>
                  <label for=fileName>Name der neuen Datei auf dem Server
                  <input #fileName type=text name=fileName>
                  <label for=file>Datei auswählen
                  <input #file type=file name=file>
                <div .modal-footer>
                  <button type=button .btn .btn-default data-dismiss=modal>Schliessen
                  <button type=submit .btn .btn-primary>Hochladen
        <div #deleteFolder .modal .fade>
          <script>
            function deleteFolder() {
              \$.ajax(
                { url: "@{AssetR $ ContentPath path}"
                , type: "DELETE"
                , success: function(result) {
                    window.location.replace("@{AssetsR $ ContentPath $ parentPath}");
                  }
                }
              );
            }
          <div .modal-dialog>
            <div .modal-content>
              <div .modal-header>
                <button type=button .close data-dismiss=modal aria-hidden=true>&times;
                <h4 .modal-title>Wirklich löschen?
              <div .modal-body>
                <p>Der aktuell angezeigte Order wird mitsamt Inhalt gelöscht!
              <div .modal-footer>
                <button type=button .btn .btn-default data-dismiss=modal>Lieber doch nicht
                <button type=submit .btn .btn-primary onClick=deleteFolder()>Löschen
        <div .col-md-8>
          <div .panel .panel-success>
            <div .panel-heading>
              <h3 .panel-title>Inhalt
            <div .panel-body>
              <div .assets .row>
              $forall asset <- assets
                <a href=@{AssetsR $ ContentPath $ map T.pack (assetPath asset)}>
                  <div .asset .col-md-2 .thumbnail style="height: 150px; margin: 10px;">
                    $if isPrefixOf "image/" $ assetType asset
                      <img src=@{AssetR $ ContentPath $ map T.pack $ assetPath asset}>
                    $else
                      <div style="height: 100px">
                        ^{icon asset}
                        <div>#{assetName asset}
        <div .col-md-4>
          <div .panel .panel-info>
            <div .panel-heading>
              <h3 .panel-title>Metadaten
            <div .panel-body>
              <dl>
                <dt>Name
                <dd>#{assetName thisAsset}
                <dt>Typ
                <dd>#{assetType thisAsset}
                <dt>Hochgeladen von
                <dd>#{assetUploadedBy thisAsset}
                <dt>Hochgeladen am
                <dd>#{show $ assetUploadedDate thisAsset}
          <div .panel .panel-danger>
            <div .panel-heading>
              <h3 .panel-title>Aktionen
            <div .panel-body>
              <a href=#addFolder role=button data-toggle=modal .btn .btn-default .btn-sm style="margin-bottom: 5px">Neuer Ordner
              <a href=#uploadFile role=button data-toggle=modal .btn .btn-default .btn-sm style="margin-bottom: 5px">Datei hochladen
              <a href=#deleteFolder role=button data-toggle=modal .btn .btn-default .btn-sm style="margin-bottom: 5px">Datei löschen
    |]

  where
    icon :: Asset -> Widget
    icon asset = if assetType asset `elem` ["application/x-directory", "unknown"]
                 then [whamlet|
                   <img src=@{StaticR gnome_folder_png} aria-hidden=true>
                 |]
                 else [whamlet|
                   <img src=@{StaticR gnome_text_x_generic_png} aria-hidden=true>
                 |]
 
auditTrail :: [T.Text] -> Widget
auditTrail (_ : rest) = do
  repo <- compRepository <$> component'
  [whamlet|
    <ul .nav .nav-pills .navbar-nav>
      <li role=presentation .active>
        <a href=@{RootR}>Startseite
      <li role=presentation .active>
        <a href=@{AssetsR $ ContentPath []}>Dateien
      ^{auditTrail' repo rest}
  |]

auditTrail' :: Repository -> [T.Text] -> Widget
auditTrail' repo path = do
  trail <- getTrail' repo path
  mapM_ (encodeTrail . navSelf) (tail trail)
  where
    encodeTrail n =
      [whamlet|
        <li role=presentation .active>
          <a href=@{url n} title=#{neTitle n}>#{neTitle n}
      |]

url = AssetsR . ContentPath . (map T.pack) . neURL

getTrail' :: Repository -> [T.Text] -> WidgetT CBGWebSite IO [Navigation NavigationEntry]
getTrail' repo path = do
  eitherNodes <- liftIO $ runEitherT $ do
    node <- getNode repo (map T.unpack path)
    getTrail node
  return $ either (const []) id eitherNodes

naviChildren :: [T.Text] -> Widget
naviChildren (_ : path) = do
  repo <- compRepository <$> component'
  trail <- getTrail' repo path
  let children = map Tree.rootLabel $ Tree.subForest $ navTree $ last trail
  [whamlet|
    <ul .nav .nav-pills>
      $forall cld <- children
        <li role=presentation .active>
          <a href=@{url cld} title=#{neTitle cld}>#{neTitle cld}
  |]
