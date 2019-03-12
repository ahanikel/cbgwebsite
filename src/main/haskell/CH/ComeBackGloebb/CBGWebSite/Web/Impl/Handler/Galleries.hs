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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Galleries where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Model.Impl.Gallery
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Layout
import CH.ComeBackGloebb.CBGWebSite.Web.Component

-- Yesod
import Yesod
import Yesod.Auth
import Text.Hamlet

-- other imports
import           Control.Exception                                  (IOException)
import           Control.Monad.Trans.Either                         (runEitherT)
import qualified Data.ByteString.UTF8                               as U8
import           Data.ByteString.Lazy                               (toStrict)
import           Data.Conduit
import qualified Data.Conduit.Binary                                as CB
import           Data.List (elemIndex, find)
import qualified Data.Text as T

component :: Handler (Component CBGWebSite)
component = do
  cs <- components <$> getYesod
  let mc = find (== "Galleries") cs
  case mc of
    Nothing -> fail "Component not found."
    Just c  -> return c

component' :: WidgetT CBGWebSite IO (Component CBGWebSite)
component' = do
  cs <- components <$> getYesod
  let mc = find (== "Galleries") cs
  case mc of
    Nothing -> fail "Component not found."
    Just c  -> return c

galleryCss :: Widget
galleryCss = toWidget
  [cassius|
    .galleryimg
      height: 320px !important
      padding: 20px
      img
        max-height: 240px !important
    .new
      background-color: light-grey
    .caption
      vertical-align: bottom
  |]

getGalleriesR :: Handler TypedContent
getGalleriesR = selectRep $
  provideRep $ do
    comp <- component
    let repo = compRepository comp
    eitherGalleries <- liftIO $ runEitherT $ list_galleries repo
    case eitherGalleries of
      Left e -> do
        $logError $ T.pack $ show e
        fail "Internal error while trying to list galleries."
      Right galleries -> do
        let
          actionPanel =
            dangerPanel
              "Aktionen"
              [whamlet|
                  <a href=#addGallery role=button data-toggle=modal .btn .btn-default .btn-sm>Neues Album
              |]
          addGalleryDialog =
            actionDialog
              "addGallery"
              "Neues Album"
              "PUT"
              (GalleryR "")
              (GalleryR "")
              [whamlet|
                <label for=addGalleryInput>Name des neuen Albums
                <input #addGalleryInput type=text name=addGalleryInput>
              |]
              [whamlet|
                <button type=button .btn .btn-default data-dismiss=modal>Schliessen
                <button type=submit .btn .btn-primary onClick=addGallery($('#addGalleryInput').val())>Erstellen
              |]
          gname = T.pack . gallery_name
        layout comp ["galleries"] $ do
          galleryCss
          [whamlet|
            <div .content>
              <div .row>
                ^{addGalleryDialog}
                <div .col-sm-9>
                  <div .panel .panel-success>
                    <div .panel-heading>
                      <h4 .panel-title>Fotoalben
                    <div .panel-body>
                      $forall g <- galleries
                        <div .col-md-3>
                          <div .galleryimg .thumbnail>
                            <a href=@{GalleryR $ gname g}>
                              $case gallery_images g
                                $of []
                                  <img src=@{FavR} alt="No images yet">
                                $of is
                                  <img src=@{ImageThumbR (gname g) (T.pack $ head is)} alt=#{gname g}>
                              <div .caption>
                                <h4>#{gname g}
                <div .col-sm-3>
                  ^{actionPanel}
          |]

getGalleryR :: T.Text -> Handler TypedContent
getGalleryR gname = selectRep $
  provideRep $ do
    comp <- component
    let repo = compRepository comp
    eitherGallery <- liftIO $ runEitherT $ gallery_read repo (T.unpack gname)
    case eitherGallery of
      Left e -> do
        $logError $ T.pack $ show e
        fail "Internal error while trying to list galleries."
      Right gallery ->
        layout comp ["gallery", gname] $ do
        galleryCss
        [whamlet|
          <h1>#{gname}
          <div .content>
            <div .row>
             $forall iname <- map T.pack $ gallery_images gallery
              <div .col-md-3>
                  <div .galleryimg .thumbnail>
                      <a href=@{GalleryImageR gname iname}>
                          <img src=@{ImageThumbR gname iname} alt=#{iname}>
             <div .col-md-3>
              <div .galleryimg .new>
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

putGalleryR :: T.Text -> Handler ()
putGalleryR gname = do
  comp <- component
  let repo = compRepository comp
  eitherResult <- liftIO $ runEitherT $ gallery_create repo (T.unpack gname)
  either (fail . show) (const $ sendResponseCreated $ GalleryR gname) eitherResult

deleteGalleryR :: T.Text -> Handler Html
deleteGalleryR _ = undefined

getGalleryImagesR :: T.Text -> Handler TypedContent
getGalleryImagesR gname = selectRep $
  provideRep $ do
    comp <- component
    let repo = compRepository comp
    eitherGallery <- liftIO $ runEitherT $ gallery_read repo (T.unpack gname)
    case eitherGallery of
      Left e -> do
        $logError $ T.pack $ show e
        fail "Internal error while trying to list galleries."
      Right gallery ->
        layout comp ["gallery", gname] [whamlet|
          <h1>#{gname}
          <table>
              <tr>
                  <th>image
              $forall iname <- map T.pack $ gallery_images gallery
                  <tr>
                      <td>
                          <a href=@{GalleryImageR gname iname}>#{iname}
        |]

getGalleryImageR :: T.Text -> T.Text -> Handler TypedContent
getGalleryImageR gname iname = selectRep $
  provideRep $ do
    comp <- component
    let repo = compRepository comp
    eitherGallery <- liftIO $ runEitherT $ gallery_read repo (T.unpack gname)
    case eitherGallery of
      Left e -> do
        $logError $ T.pack $ show e
        fail "Internal error while trying to list galleries."
      Right gallery -> do
        let images     = gallery_images gallery
            Just imgno = elemIndex (T.unpack iname) images
            imgprev    = if imgno < 1
                         then []
                         else take 1 $ drop (imgno - 1) images
            imgnext    = take 1 $ drop (imgno + 1) images
        layout comp ["gallery", gname, iname] [whamlet|
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

postGalleryImageR :: T.Text -> T.Text -> Handler Html
postGalleryImageR _ _ = undefined

deleteGalleryImageR :: T.Text -> T.Text -> Handler Html
deleteGalleryImageR _ _ = undefined

getImageR :: T.Text -> T.Text -> Handler ()
getImageR gname iname = do
    repo        <- compRepository <$> component
    eitherImage <- liftIO $ runEitherT $ image_read repo (T.unpack gname) (T.unpack iname)
    case eitherImage of
        Left e -> do
            $logError $ T.pack $ show e
            notFound
        Right image -> sendFile (U8.fromString $ image_type image) (image_blob image)

getImageSmallR :: T.Text -> T.Text -> Handler ()
getImageSmallR gname iname = do
    repo        <- compRepository <$> component
    eitherImage <- liftIO $ runEitherT $ image_read repo (T.unpack gname) (T.unpack iname)
    case eitherImage of
        Left e -> do
            $logError $ T.pack $ show e
            notFound
        Right image -> sendFile (U8.fromString $ image_type image) (image_small image)

getImageThumbR :: T.Text -> T.Text -> Handler ()
getImageThumbR gname iname = do
    repo        <- compRepository <$> component
    eitherImage <- liftIO $ runEitherT $ image_read repo (T.unpack gname) (T.unpack iname)
    case eitherImage of
        Left e -> do
            $logError $ T.pack $ show e
            notFound
        Right image -> sendFile (U8.fromString $ image_type image) (image_thumb image)

postUploadImageR :: T.Text -> Handler Html
postUploadImageR gname = do
  (_, files) <- runRequestBody
  mauthUser <- maybeAuthId
  case mauthUser of
    Nothing ->
      permissionDenied ""
    Just userName -> do
      comp <- component
      let repo = compRepository comp
          upload (_, file) = do
            let iname = fileName file
                type' = fileContentType file
            bytes <- runConduit $ fileSource file $$ CB.sinkLbs
            eitherResult <- liftIO $
              runEitherT $ image_write repo
                                       (T.unpack gname)
                                       (T.unpack iname)
                                       (T.unpack type')
                                       (T.unpack userName)
                                       (toStrict bytes)
            case eitherResult of
              Left e -> do
                $logError $ T.pack $ show e
                fail "Internal error while trying to save image."
              Right _ ->
                return ()
      mapM_ upload files
      layout comp ["upload", "image"] [whamlet|
        <h1>File upload status
      |]
 
auditTrail :: [T.Text] -> Widget
auditTrail ("galleries" : _) = do
  [whamlet|
    <ul .nav .nav-pills .navbar-nav>
      <li role=presentation .active>
        <a href=@{RootR}>Startseite
      <li role=presentation .active>
        <a href=@{GalleriesR}>Fotoalben
  |]

auditTrail ("gallery" : name : _) = do
  [whamlet|
    <ul .nav .nav-pills .navbar-nav>
      <li role=presentation .active>
        <a href=@{RootR}>Startseite
      <li role=presentation .active>
        <a href=@{GalleriesR}>Fotoalben
      <li role=presentation .active>
        <a href=@{GalleryR name}>#{name}
  |]

auditTrail ("upload" : "image" : _) = return ()

auditTrail' :: Repository -> String -> [T.Text] -> Widget
auditTrail' _ _ ("gallery" : name : _) = do
  [whamlet|
    <li>
      <a href=@{GalleryR name}>#{name}
  |]
auditTrail' _ _ _ = return ()

naviChildren :: [T.Text] -> Maybe Widget
naviChildren _ = Nothing
