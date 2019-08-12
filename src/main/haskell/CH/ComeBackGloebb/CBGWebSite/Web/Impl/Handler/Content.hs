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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Content where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Model.Impl.Navigation
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Layout
import CH.ComeBackGloebb.CBGWebSite.Web.Component

-- Yesod
import Yesod
import Yesod.Auth
import Text.Hamlet

-- other imports
import           Control.Monad                                      (liftM)
import           Control.Monad.Except                               (runExceptT)
import qualified Data.ByteString.UTF8                               as U8
import           Data.List                                          (find, sort, sortBy)
import           Data.Maybe                                         (fromMaybe)
import           Data.Ord                                           (comparing)
import qualified Data.Text                                          as T
import qualified Data.Tree                                          as Tree
import Debug.Trace

component :: Handler (Component CBGWebSite)
component = do
  cs <- components <$> getYesod
  let mc = find (== "Content") cs
  case mc of
    Nothing -> fail "Component not found."
    Just c  -> return c

component' :: WidgetT CBGWebSite IO (Component CBGWebSite)
component' = do
  mcomponent <- find (== "Content") <$> components <$> getYesod
  case mcomponent of
    Nothing ->
      fail "Component not found."
    Just component -> return component

getContentR :: ContentPath -> Handler Html
getContentR (ContentPath pieces) = do
  repo <- compRepository <$> component
  contentLayout ("content" : pieces) $ do
    eitherNode <- liftIO $ runExceptT $ do
        let url = map T.unpack pieces
        getNode repo url
    case eitherNode of
        Left _ -> notFound
        Right node -> do
          res <- liftIO $ runExceptT $ liftM U8.toString $ getProperty node "text.html"
          prop <- either (fail.show) return res
          toWidget $ preEscapedToMarkup prop

getEditContentR :: ContentPath -> Handler Html
getEditContentR (ContentPath pieces) = do
    let url = map T.unpack pieces
    repo <- compRepository <$> component
    eitherNode <- liftIO $ runExceptT $ getNode repo url
    case eitherNode of
        Left _ -> case url of
          [] -> notFound
          _  -> do
            editLayout
              ("content" : pieces)
              (toWidget $ toHtml T.empty)
              (toWidget $ toHtml T.empty)
        Right node -> do
          let propNames   = node_props node
          res            <- liftIO $ runExceptT $ mapM (getProperty node) (trace (show propNames) propNames)
          propValues     <- either (fail . show) (mapM (return . U8.toString)) res
          let props       = zip propNames propValues :: [(String, String)]
              textHtml    = snd <$> find ((== "text.html") . fst) props
              contentBody = toWidget $ toHtml $ fromMaybe "" textHtml
              props'      = filter ((/= "text.html") . fst) props
              props''     = map createForm props'
              createForm (p,v) = [whamlet|
                <span>
                  ^{label p}
                  ^{element p v}
                  <button .removeProp type=button onClick=removeProp('#{p}')>
                    <span .glyphicon .glyphicon-minus-sign>
              |]
              label p = [whamlet|
                <label for=#{"prop_" ++ p}>#{p}
              |]
              element p v = [whamlet|
                <input type=text id=#{"prop_" ++ p} name=#{p} value=#{v}>
              |]
              propertiesWidget = [whamlet|
                <script>
                  function addProp(p) {
                    \$('.ppRow:last-child').before('<div class="ppRow"><span><label for="prop_"' + p + '>' + p + '</label><input type="text" id="' + p + '" name = "' + p + '" value=""><button class="removeProp" type="button" onClick=\'removeProp(\"' + p + '\")\'><span class="glyphicon glyphicon-minus-sign"></span></button></span></div>');
                  }
                <form #pageProperties>
                  $forall p <- props''
                    <div .ppRow>
                      ^{p}
                  <div .ppRow>
                    <span #addProp>
                      <input #newProp type=text>
                      <button .addProp type=button onClick=addProp($('#newProp').val())>
                        <span .glyphicon .glyphicon-plus-sign>
              |]
          editLayout ("content" : pieces) contentBody propertiesWidget

postEditContentR :: ContentPath -> Handler Html
postEditContentR (ContentPath pieces) = do
    let url = map T.unpack pieces
    repo <- compRepository <$> component
    eitherNode <- liftIO $ runExceptT $ getNode repo url
    node <- case eitherNode of
        Left _ -> case url of
          [] -> notFound
          _  -> do
            let name = last url
                node = Node name url [] repo
            _ <- liftIO $ runExceptT $ writeNode node
            return node
        Right node -> return node
    pagebody <- runInputPost $ ireq textField "body"
    reqbody <- filter ((/= "body") . fst) <$> fst <$> runRequestBody
    res <- liftIO $ runExceptT $ do
      writeProperty node "text.html" (U8.fromString $ T.unpack pagebody)
      mapM_ (\(pname, pvalue) -> writeProperty node (T.unpack pname) (U8.fromString $ T.unpack pvalue)) $ makePairs reqbody
    case res of
      Left err -> fail $ show err
      Right () -> return ()
    withUrlRenderer [hamlet||]
  where
    -- [("props[0][name]","title"),("props[0][value]","Some title")] -> [("title", "Some title")]
    makePairs params = pairs $ map snd $ sortBy (comparing fst) params
    pairs [] = []
    pairs (one : two : rest) = (one, two) : pairs rest

deleteEditContentR :: ContentPath -> Handler ()
deleteEditContentR (ContentPath pieces) = do
    let url = map T.unpack pieces
    repo <- compRepository <$> component
    eitherRes <- liftIO $ runExceptT $ do
      getNode repo url >>= deleteNode
    case eitherRes of
      Left _ -> notFound
      Right _ -> return ()

contentLayout :: [T.Text] -> Widget -> Handler Html
contentLayout path body = do
  comp <- component
  layout comp path body

editLayout :: [T.Text] -> Widget -> Widget -> Handler Html
editLayout path contentBody propsPanel = contentLayout ("edit" : path) $ do
  let contentPanel =
        successPanel
          "Inhalt"
          [whamlet|
            <ul .nav .nav-tabs role=tablist>
                <li role=presentation .active>
                    <a href=#content aria-controls=content role=tab data-toggle=tab>Content
                <li role=presentation>
                    <a href=#properties aria-controls=properties role=tab data-toggle=tab>Properties
            <div .tab-content>
                <div role=tabpanel .tab-pane .active id=content>
                    ^{ckEditor contentBody}
                <div role=tabpanel .tab-pane id=properties>
                    ^{propsPanel}
          |]
      metadataPanel =
        infoPanel
          "Eigenschaften"
          [whamlet|
          |]
      saveButton :: Widget
      saveButton = do
        toWidget [julius|
          function restoreSaveButton() {
              $('#savebutton').removeClass('btn-success')
                              .removeClass('btn-danger')
                              .addClass('btn-primary')
                              .text('Save');
          }
          function successSaveButton() {
              $('#savebutton').removeClass('btn-primary')
                              .removeClass('btn-danger')
                              .addClass('btn-success')
                              .text('Saved!');
              window.setTimeout(restoreSaveButton, 5000);
          }
          function errorSaveButton() {
              $('#savebutton').removeClass('btn-success')
                              .removeClass('btn-primary')
                              .addClass('btn-danger')
                              .text('Error while saving! Retry?');
              window.setTimeout(restoreSaveButton, 5000);
          }
          $('#savebutton').click(function(event) {
              $.post('#', {
                  body: CKEDITOR.instances.body.getData()
                , props: $('#pageProperties').serializeArray()
              }, function(ret) {})
              .success(successSaveButton)
              .error(errorSaveButton);
          });
        |]
        [whamlet|
          <button #savebutton type=button .btn .btn-primary>Speichern
        |]
      newPageButton :: Widget
      newPageButton = do
        redirectDialog
          "newPageDialog"
          "Neue Seite erstellen"
          (EditContentR $ ContentPath $ tail path)
          [whamlet|
            <label for=newPageInput>Name der neuen Seite
            <input #newPageInput type=text name=newPageInput>
          |]
          [whamlet|
            <button type=button .btn .btn-default data-dismiss=modal>Schliessen
            <button type=submit .btn .btn-primary onClick=editNewPage($('#newPageInput').val())>Erstellen
          |]
        [whamlet|
          <button href=#newPageDialog role=button data-toggle=modal .btn .btn-primary>Neue Seite
        |]
      deletePageButton :: Widget
      deletePageButton = do
        let path' = tail path
        if path' == [] then return ()
        else actionDialog
          "deletePage"
          "Wirklich löschen?"
          "DELETE"
          (EditContentR $ ContentPath path')
          (EditContentR $ ContentPath $ init path')
          [whamlet|
            <p>Die aktuell angezeigte Seite wird gelöscht!
          |]
          [whamlet|
            <button type=button .btn .btn-default data-dismiss=modal>Lieber doch nicht
            <button type=submit .btn .btn-primary onClick=deletePage()>Löschen
          |]
        [whamlet|
          <button href=#deletePage role=button data-toggle=modal .btn .btn-primary>Seite löschen
        |]
      actionPanel =
        dangerPanel
          "Aktionen"
          [whamlet|
            ^{saveButton}
            ^{newPageButton}
            ^{deletePageButton}
          |]
  [whamlet|
    <div .row>
      <div .col-md-8>
        ^{contentPanel}
      <div .col-md-4>
        ^{metadataPanel}
        ^{actionPanel}
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

contentNavigationFromRoot :: Widget
contentNavigationFromRoot = contentNavigation ["content"]

editContentNavigationFromRoot :: Widget
editContentNavigationFromRoot = contentNavigation ["edit","content"]

contentNavigation :: [T.Text] -> Widget
contentNavigation path = do
  repo                   <- compRepository <$> component'
  let path'               = map T.unpack path
      (urlFunc, repoPath) =
        case path' of
          ("content" : rest)          -> (getContentUrlFromURL, rest)
          ("edit" : "content" : rest) -> (getEditContentUrlFromURL, rest)
          _                           -> fail "no content node"
  eitherNodes <- liftIO $ runExceptT $ getNavigation repo (urlFromStrings repoPath) 1
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

getContentUrlFromURL :: URL -> String
getContentUrlFromURL = ("/content/" ++) . urlToString

getEditContentUrlFromURL :: URL -> String
getEditContentUrlFromURL = ("/edit/content/" ++) . urlToString

auditTrail :: [T.Text] -> Widget
auditTrail ("edit" : "content" : rest) = do
  repo <- compRepository <$> component'
  [whamlet|
    <ul .nav .nav-pills .navbar-nav>
      <li role=presentation .active>
        <a href=@{EditContentR $ ContentPath []}>Bearbeiten
      ^{auditTrail' EditContentR repo rest}
  |]

auditTrail (prefix : rest) = do
  repo <- compRepository <$> component'
  [whamlet|
    <ul .nav .nav-pills .navbar-nav>
      ^{auditTrail' ContentR repo rest}
  |]

auditTrail' :: (ContentPath -> Route CBGWebSite) -> Repository -> [T.Text] -> Widget
auditTrail' linkFunc repo path = do
  trail <- getTrail' repo path
  mapM_ encodeTrail trail
  where
    encodeTrail n = do
      let ne = navSelf n
      [whamlet|
        <li role=presentation .active>
          <a href=@{url linkFunc ne} role=button title=#{neTitle ne}>#{neTitle ne}
      |]

url linkFunc = linkFunc . ContentPath . (map T.pack) . neURL

getTrail' :: Repository -> [T.Text] -> WidgetT CBGWebSite IO [Navigation NavigationEntry]
getTrail' repo path = do
  eitherNodes <- liftIO $ runExceptT $ do
    node <- getNode repo (map T.unpack path)
    getTrail node
  return $ either (const []) id eitherNodes

naviChildren :: [T.Text] -> Maybe Widget
naviChildren ("edit" : "content" : rest) = Just $ naviChildren' EditContentR rest
naviChildren ("content"          : rest) = Just $ naviChildren' ContentR rest

naviChildren' :: (ContentPath -> Route CBGWebSite) -> [T.Text] -> Widget
naviChildren' linkFunc path = do
  repo <- compRepository <$> component'
  trail <- getTrail' repo path
  let children =
        case trail of
          [] -> []
          _  -> map Tree.rootLabel $ Tree.subForest $ navTree $ last trail
  [whamlet|
    <ul .nav .nav-pills .nav-stacked>
      $forall cld <- children
        <li role=presentation .active>
          <a href=@{url linkFunc cld} title=#{neTitle cld}>#{neTitle cld}
  |]
