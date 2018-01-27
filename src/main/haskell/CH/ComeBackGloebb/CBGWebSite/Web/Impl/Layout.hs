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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Layout ( layout
                                                    , actionDialog
                                                    , fileDialog
                                                    , redirectDialog
                                                    , successPanel
                                                    , infoPanel
                                                    , dangerPanel
                                                    ) where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Web.Component
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation

-- Yesod
import Yesod
import Yesod.Auth
import Text.Hamlet
import Text.Cassius

-- other imports
import qualified Data.Text as T

-- exported
layout :: Component CBGWebSite -> [T.Text] -> Widget -> Handler Html
layout comp path widget = do
  pageContent  <- widgetToPageContent (widget >> toWidget css)
  maybeAuthId' <- maybeAuthId
  trail        <- widgetToPageContent $ compNaviTrail comp path >> loginWidget maybeAuthId'
  children     <- case compNaviChildren comp path of
    Nothing -> return Nothing
    Just c  -> widgetToPageContent c >>= return . Just
  withUrlRenderer $ template trail children pageContent

template trail mChildren pageContent = [hamlet|
$doctype 5
<html lang=en>
  <head>
    ^{htmlHead pageContent}
  <body>
    ^{htmlBody trail mChildren pageContent}
|]

css = [cassius|
  #main-content
    padding-left: 3em
    padding-right: 2em
    h1
      margin-top: 2ex
    h2
      margin-top: 2ex
      margin-bottom: 2ex
    h3
      margin-top: 2ex
      margin-bottom: 2ex
  .navbar
    background-color: dodgerblue
    margin-top: 20px
  .navbar-header
    height: 130px
  .header
    ul.nav
      margin-left: 20px
      vertical-align: middle
      padding-top: 40px
      padding-left: 60px
      padding-right: 20px
  #sidebar
    padding-top: 2ex
    li
      padding-left: 2em
      padding-right: 2em
      margin-bottom: 2px
  #cbg-sticker
    width: 300px
    padding: 20px
    z-index: -1
    position: relative
  #calendar
    .day
      height: 100px
      width: 80px
      border: 1px solid grey
      border-radius: 4px
      float: left
      margin: 10px
      padding: 5px
      p
        background-color: yellow
  #event-form
    margin: 10px
  @media (min-width: 768px)
    .pull-right-lg
      float: right !important
  @media (max-width: 767px)
    .pull-right-lg
      margin-bottom: 40px
  td
    padding-right: 1em
  td.phone, td.mobile
      white-space: nowrap
|]

htmlHead pageContent = [hamlet|
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <meta charset=utf-8>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name=viewport content="width=device-width, initial-scale=1">
    <meta name=description content="D'Websytte vom Come Back Glöbb Allschwil">
    <meta name=author content="Come Back Glöbb Allschwil">
    <link rel=icon href=@{FavR}>
    <title>#{pageTitle pageContent}
    <link rel=stylesheet href=@{StaticR bootstrap_css}>
    <link rel=stylesheet href=@{StaticR ie10_viewport_bug_workaround_css}>
    <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js">
      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js">
    <![endif]-->
    <link rel=stylesheet href=@{StaticR jquery_ui_css}>
    <script src=@{StaticR jquery_js}>
    <script src=@{StaticR jquery_ui_js}>
    <script src=@{StaticR bootstrap_js}>
    <script src=@{StaticR ie10_viewport_bug_workaround_js}>
    <script src=@{StaticR angular_min_js}>
    ^{pageHead pageContent}
|]

htmlBody trail mChildren pageContent =
  [hamlet|
    <div .container-fluid>
      <div .header .clearfix>
        <nav .navbar>
          <div .navbar-header>
            <a .navbar-brand href=@{RootR}>
              <img #cbg-logo .img-responsive src=@{StaticR cbg_logo_png}>
          ^{pageBody trail}
    <div #content-area .container-fluid>
      <div .row>
        $case mChildren
          $of Just children
            <div #sidebar .col-sm-3 .col-lg-2>
              ^{pageBody children}
            <div #main-content .col-sm-9 .col-lg-7>
              ^{pageBody pageContent}
          $of Nothing
            <div #main-content .col-sm-12 .col-lg-9>
              ^{pageBody pageContent}
        <div .col-sm-0 .col-lg-3>
          <img #cbg-sticker .img-responsive src="/static/assets/images/gloebb-gross.png">
  |]

loginWidget :: Maybe (AuthId CBGWebSite) -> Widget
loginWidget maybeAuthId' = do
  case maybeAuthId' of
    Just _ ->
      [whamlet|
        <ul .nav .nav-pills .navbar-nav .pull-right-lg>
          <li role=presentation .active>
            <a href=# .dropdown-toggle data-toggle=dropdown role=button aria-haspopup=true aria-expanded=false title="Mitglieder">
              Mitglieder
              <span .caret>
            <ul .dropdown-menu>
              <li>
                <a href=@{EditContentR $ ContentPath []}>Seiten bearbeiten
                <a href=@{GalleriesR}>Fotos
                <a href=@{AssetsR $ ContentPath []}>Dateien
                <a href=@{MemberCalendarR}>Kalender
                <a href=@{MemberListR}>Mitgliederliste
          <li role=presentation .active>
            <a href=@{AuthR LogoutR} title="Abmelden">Abmelden
      |]
    Nothing ->
      [whamlet|
        <ul .nav .nav-pills .navbar-nav .pull-right-lg>
          <li role=presentation .active>
            <a href=@{AuthR LoginR} title="Mitglieder">Mitglieder
      |]

-- exported
actionDialog :: T.Text -> T.Text -> T.Text -> Route CBGWebSite -> Route CBGWebSite -> Widget -> Widget -> Widget
actionDialog id title method url redirectUrl body footer = [whamlet|
        <div ##{id} .modal .fade>
          <script>
            function #{id}(name) {
              var url = name ? "/" + name : ""
              \$.ajax(
                { url: "@{url}".replace(/\/-$/, "") + url
                , type: "#{method}"
                , success: function(result) {
                    window.location.assign("@{redirectUrl}".replace(/\/-$/, "") + url);
                  }
                }
              );
            }
          <div .modal-dialog>
            <div .modal-content>
              <div .modal-header>
                <button type=button .close data-dismiss=modal aria-hidden=true>&times;
                <h4 .modal-title>#{title}
              <div .modal-body>
                ^{body}
              <div .modal-footer>
                ^{footer}
|]

-- exported
fileDialog :: T.Text -> T.Text -> Route CBGWebSite -> Widget -> Widget -> Widget
fileDialog id title url body footer = [whamlet|
        <div ##{id} .modal .fade>
          <div .modal-dialog>
            <div .modal-content>
              <form method=post action=@{url} enctype="multipart/form-data">
                <div .modal-header>
                  <button type=button .close data-dismiss=modal aria-hidden=true>&times;
                  <h4 .modal-title>#{title}
                <div .modal-body>
                  ^{body}
                <div .modal-footer>
                  ^{footer}
|]

-- exported
redirectDialog :: T.Text -> T.Text -> Route CBGWebSite -> Widget -> Widget -> Widget
redirectDialog id title url body footer = do
  toWidget [julius|
    function editNewPage(name) {
      window.location.assign("@{url}/" + name);
    }
  |]
  [whamlet|
    <div ##{id} .modal .fade>
      <div .modal-dialog>
        <div .modal-content>
          <div .modal-header>
            <button type=button .close data-dismiss=modal aria-hidden=true>&times;
            <h4 .modal-title>#{title}
          <div .modal-body>
            ^{body}
          <div .modal-footer>
            ^{footer}
  |]

panel :: T.Text -> Widget -> Widget
panel title body = [whamlet|
  <div .panel-heading>
    <h4 .panel-title>#{title}
  <div .panel-body>
    ^{body}
|]

-- exported
successPanel :: T.Text -> Widget -> Widget
successPanel title body = [whamlet|
  <div .panel .panel-success>
    ^{panel title body}
|]

-- exported
infoPanel :: T.Text -> Widget -> Widget
infoPanel title body = [whamlet|
  <div .panel .panel-info>
    ^{panel title body}
|]
-- exported
dangerPanel :: T.Text -> Widget -> Widget
dangerPanel title body = [whamlet|
  <div .panel .panel-danger>
    ^{panel title body}
|]
