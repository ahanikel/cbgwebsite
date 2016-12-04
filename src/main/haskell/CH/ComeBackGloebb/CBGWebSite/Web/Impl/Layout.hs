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

-- other imports
import qualified Data.Text as T

layout :: Component CBGWebSite -> [T.Text] -> Widget -> Handler Html
layout comp path widget = do
  let compNavi =  compNavigation comp
  pageContent  <- widgetToPageContent widget
  maybeAuthId' <- maybeAuthId
  trail        <- widgetToPageContent $ compNavi path >> loginWidget maybeAuthId'
  withUrlRenderer $(hamletFile "src/main/haskell/CH/ComeBackGloebb/CBGWebSite/Web/Impl/newlayout.hamlet")

loginWidget :: Maybe (AuthId CBGWebSite) -> Widget
loginWidget maybeAuthId' = do
  case maybeAuthId' of
    Just _ ->
      [whamlet|
        <ul .nav .nav-pills .navbar-right>
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
        <ul .nav .nav-pills .navbar-right>
          <li role=presentation .active>
            <a href=@{AuthR LoginR} title="Mitglieder">Mitglieder
      |]
