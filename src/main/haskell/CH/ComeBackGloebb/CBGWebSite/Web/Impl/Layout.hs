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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Layout (cbgLayout, editLayout) where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Model.Impl.Gallery
import CH.ComeBackGloebb.CBGWebSite.Model.Impl.Navigation
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation

-- Yesod
import Yesod
import Yesod.Auth
import Text.Hamlet

-- other imports
import           Control.Exception                                  (IOException)
import           Control.Monad.Trans.Either                         (left,
                                                                     runEitherT)
import           Data.List                                          (sort)
import qualified Data.Text as T
import qualified Data.Tree                                          as Tree

cbgLayout :: [T.Text] -> Widget -> Handler Html
cbgLayout path widget = do pageContent  <- widgetToPageContent widget
                           maybeAuthId' <- maybeAuthId
                           navi         <- widgetToPageContent $ navigationWidget maybeAuthId' path
                           trail        <- widgetToPageContent $ auditTrail       path
                           withUrlRenderer $(hamletFile "src/main/haskell/CH/ComeBackGloebb/CBGWebSite/Web/Impl/layout.hamlet")

editLayout :: [T.Text] -> Widget -> Handler Html
editLayout path = cbgLayout ("edit" : path) . editPage

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

navigationWidget :: Maybe authId -> [T.Text] -> Widget
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
                                                           <a href=@{EditContentR $ ContentPath []} title=Webseiten>Webseiten
                                                       <li .leaf>
                                                           <a href=@{MemberCalendarR} title=Kalender>Kalender
                                                       <li .leaf>
                                                           <a href=@{MemberListR} title=Mitgliederliste>Mitgliederliste
                                                       <li .leaf>
                                                           <a href=@{AssetsR $ ContentPath []} title=Dateien>Dateien
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
                               ("assets" : _) ->
                                   [whamlet|
                                             <ul .menu>
                                               <li .collapsed>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                               <li .expanded>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                                   <ul .menu>
                                                       <li .leaf>
                                                           <a href=@{EditContentR $ ContentPath []} title=Webseiten>Webseiten
                                                       <li .leaf>
                                                           <a href=@{MemberCalendarR} title=Kalender>Kalender
                                                       <li .leaf>
                                                           <a href=@{MemberListR} title=Mitgliederliste>Mitgliederliste
                                                       <li .expanded>
                                                           <a href=@{AssetsR $ ContentPath []} title=Dateien>Dateien
                                                           <ul .menu>
                                                               <li .leaf>&rarr;
                                                       <li .leaf>
                                                           <a href=@{AssetR $ ContentPath ["Verein", "Statuten", "Statuten.pdf"]} title=Statuten>Statuten
                                                       <li .collapsed>
                                                           <a href=@{GalleriesR} title=Fotoalben>Fotoalben
                                   |]
                               ("edit" : "content" : _) ->
                                   [whamlet|
                                             <ul .menu>
                                               <li .collapsed>
                                                   <a href=@{RootR} title=Willkommen>Willkommen
                                               <li .expanded>
                                                   <a href=@{MembersR} title="Mitglieder">Mitglieder
                                                   <ul .menu>
                                                       <li .expanded>
                                                           <a href=@{EditContentR $ ContentPath []} title=Webseiten>Webseiten
                                                           ^{editContentNavigationFromRoot}
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

editContentNavigationFromRoot :: Widget
editContentNavigationFromRoot = contentNavigation ["edit","content"]

contentNavigation :: [T.Text] -> Widget
contentNavigation path = do app         <- getYesod
                            let path'    =  map T.unpack path
                            let (urlFunc, repoPath) =  case path' of
                                                ("content" : rest)          -> (getContentUrlFromURL, rest)
                                                ("edit" : "content" : rest) -> (getEditContentUrlFromURL, rest)
                                                _                           -> fail "no content node"
                            eitherNodes <- liftIO $ runEitherT $ getNavigation (contentRepo app) (urlFromStrings repoPath) 2
                            case eitherNodes of
                                Left _ ->
                                  return ()
                                Right navi ->
                                  renderNavi urlFunc navi

renderNavi :: (URL -> String) -> Navigation NavigationEntry -> Widget
renderNavi toUrlFunc Navigation {..} = do
  let self     = Tree.rootLabel navTree
      siblings = sort (self : navSiblings)
      children = Tree.subForest navTree
      url      = toUrlFunc . neURL
  [whamlet|
    <ul .menu>
      $forall entry <- siblings
        $if entry == self
          <li .collapsed>
            <a href=#{url self} title=#{neTitle self}>#{neTitle self}
            $if (not . null) children
              $forall child <- children
                ^{renderTree toUrlFunc child}
        $else
          <li .collapsed>
            <a href=#{url entry} title=#{neTitle entry}>#{neTitle entry}
  |]

renderTree :: (URL -> String) -> Tree.Tree NavigationEntry -> Widget
renderTree toUrlFunc tree = do
  let url  = toUrlFunc . neURL
      self = Tree.rootLabel tree
      children = Tree.subForest tree
  [whamlet|
    <ul .menu>
      <li .expanded>
        <a href=#{url self} title=#{neTitle self}>#{neTitle self}
        $if (not . null) children
          $forall child <- children
            ^{renderTree toUrlFunc child}
  |]

auditTrail :: [T.Text] -> Widget
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

galleryNavigation :: [T.Text] -> Widget
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

getEditContentUrlFromURL :: URL -> String
getEditContentUrlFromURL = ("/edit/content/" ++) . urlToString
