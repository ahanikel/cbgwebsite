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

-- Yesod
import Yesod
import Yesod.Auth

-- other imports
import           Control.Monad                                      (liftM)
import           Control.Monad.Trans.Either                         (left,
                                                                     runEitherT)
import qualified Data.ByteString.Lazy.UTF8                          as UL8
import qualified Data.ByteString.UTF8                               as U8
import           Data.Conduit
import qualified Data.Conduit.Binary                                as CB
import           Data.List (elemIndex)
import qualified Data.Text as T

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

getGalleryR :: T.Text -> Handler TypedContent
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

postGalleryR :: T.Text -> Handler Html
postGalleryR _ = undefined

deleteGalleryR :: T.Text -> Handler Html
deleteGalleryR _ = undefined

getGalleryImagesR :: T.Text -> Handler TypedContent
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


getGalleryImageR :: T.Text -> T.Text -> Handler TypedContent
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


postGalleryImageR :: T.Text -> T.Text -> Handler Html
postGalleryImageR _ _ = undefined

deleteGalleryImageR :: T.Text -> T.Text -> Handler Html
deleteGalleryImageR _ _ = undefined

getImageR :: T.Text -> T.Text -> Handler ()
getImageR gname iname = do
    app         <- getYesod
    eitherImage <- liftIO $ runEitherT $ image_read (galleryRepo app) (T.unpack gname) (T.unpack iname)
    case eitherImage of
        Left e -> do
            $logError $ T.pack $ show e
            notFound
        Right image -> sendFile (U8.fromString $ image_type image) (image_blob image)

getImageSmallR :: T.Text -> T.Text -> Handler ()
getImageSmallR gname iname = do
    app         <- getYesod
    eitherImage <- liftIO $ runEitherT $ image_read (galleryRepo app) (T.unpack gname) (T.unpack iname)
    case eitherImage of
        Left e -> do
            $logError $ T.pack $ show e
            notFound
        Right image -> sendFile (U8.fromString $ image_type image) (image_small image)

getImageThumbR :: T.Text -> T.Text -> Handler ()
getImageThumbR gname iname = do
    app         <- getYesod
    eitherImage <- liftIO $ runEitherT $ image_read (galleryRepo app) (T.unpack gname) (T.unpack iname)
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
