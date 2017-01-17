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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Layout (layout) where

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

layout :: Component CBGWebSite -> [T.Text] -> Widget -> Handler Html
layout comp path widget = do
  pageContent  <- widgetToPageContent (widget >> toWidget css)
  maybeAuthId' <- maybeAuthId
  trail        <- widgetToPageContent $ compNaviTrail comp path >> loginWidget maybeAuthId'
  children     <- widgetToPageContent $ compNaviChildren comp path
  withUrlRenderer $ template trail children pageContent

template trail children pageContent = [hamlet|
$doctype 5
<html lang=en>
  <head>
    ^{htmlHead pageContent}
  <body>
    ^{htmlBody trail children pageContent}
|]

css = [cassius|
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
      padding-right: 20px
  #sidebar
    li
      margin-bottom: 2px
  #cbg-sticker
    width: 300px
    padding: 20px
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
    <script src=@{StaticR jquery_js}>
    <script src=@{StaticR bootstrap_js}>
    <script src=@{StaticR ie10_viewport_bug_workaround_js}>
    ^{pageHead pageContent}
|]

htmlBody trail children pageContent = [hamlet|
    <div .container-fluid>
      <div .header .clearfix>
        <nav .navbar>
          <div .navbar-header>
            <a .navbar-brand href=@{RootR}>
              <img #cbg-logo .img-responsive src=@{StaticR cbg_logo_png}>
          ^{pageBody trail}
    <div #content-area .container-fluid>
      <div .row>
        <div #sidebar .col-md-2>
          ^{pageBody children}
        <div #main-content .col-md-7>
          ^{pageBody pageContent}
        <div .col-md-3>
          <img #cbg-sticker .img-responsive src="/static/assets/images/gloebb-gross.png">
|]

loginWidget :: Maybe (AuthId CBGWebSite) -> Widget
loginWidget maybeAuthId' = do
  case maybeAuthId' of
    Just _ ->
      [whamlet|
        <ul .nav .nav-pills .pull-right>
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
        <ul .nav .nav-pills .pull-right>
          <li role=presentation .active>
            <a href=@{AuthR LoginR} title="Mitglieder">Mitglieder
      |]
