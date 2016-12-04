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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Root where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation

-- Yesod
import Yesod
import Yesod.Auth
import Text.Hamlet

import qualified Data.Text as T

getRootR :: Handler ()
getRootR = redirect ("/content" :: String)

getFavR :: Handler ()
getFavR = sendFile "image/png" "src/main/haskell/CH/ComeBackGloebb/CBGWebSite/Web/static/cbg-favicon.png"

getMembersR :: Handler Html
getMembersR = layout ["members"] [whamlet|
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

layout :: [T.Text] -> Widget -> Handler Html
layout path widget = do
  pageContent  <- widgetToPageContent widget
  maybeAuthId' <- maybeAuthId
  trail        <- widgetToPageContent $ auditTrail path
  withUrlRenderer $(hamletFile "src/main/haskell/CH/ComeBackGloebb/CBGWebSite/Web/Impl/newlayout.hamlet")

auditTrail ("members" : rest) = do
  [whamlet|
    <ul .nav .navbar-nav>
      <li>
        <a href=@{RootR}>Startseite
      <li>
        <a href=@{MembersR}>Mitglieder
  |]

--    <div .jumbotron>
--      <div .container>
--        <div .row>
--          <div .col-md-8>
--            <h1>Willkommen!
--            <p>
--              ...auf der Website des Come Back Glöbb Allschwil.
--              Wir möchten Ihnen auf dieser Seite unseren Verein näher bringen
--              und Sie über unsere aktuellen Tätigkeiten auf dem Laufenden halten.
--            <p>
--            <a .btn .btn-primary .btn-lg href=# role=button>Mehr erfahren »
--          <div .col-md-4>
--            <img .img-responsive src=@{StaticR gloebb_transparent_png}>
