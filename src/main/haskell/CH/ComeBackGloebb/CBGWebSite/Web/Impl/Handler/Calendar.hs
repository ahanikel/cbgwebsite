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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Calendar where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Model.Impl.Calendar
import CH.ComeBackGloebb.CBGWebSite.Model.Impl.Event
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Layout
import CH.ComeBackGloebb.CBGWebSite.Web.Component

-- Yesod
import Yesod
import Yesod.Auth
import Text.Hamlet
import Network.Wai                                                  (strictRequestBody)

-- other imports
import           Control.Monad                                      (liftM)
import           Control.Monad.Trans.Either                         (runEitherT)
import           Data.Aeson                                         (decode)
import           Data.DateTime
import           Data.Function                                      (on)
import           Data.List                                          (partition, sortBy, find)
import           Data.Maybe                                         (fromMaybe, fromJust)
import qualified Data.Text as T
import qualified Data.UUID                                          as U
import           Debug.Trace

component = do
  cs <- components <$> getYesod
  let mc = find (== "Calendar") cs
  case mc of
    Nothing -> fail "Component not found."
    Just c  -> return c

getMemberCalendarR :: Handler ()
getMemberCalendarR = do
    (year, month, _) <- liftIO $ liftM toGregorian' getCurrentTime
    redirect $ MemberCalendarMR (fromInteger year) month

getMemberCalendarMR :: Int -> Int -> Handler TypedContent
getMemberCalendarMR year month = do
  comp <- component
  let repo = compRepository comp
  if month < 1 || month > 12
  then notFound
  else selectRep $ do
    provideRep $ do
      events <- sortBy (compare `on` evStartDate) <$> getEvents repo
      let cal              = calendarForMonth year month         :: [[DateTime]]
          calWithoutEvents = map (map (\d -> (d, []))) cal       :: [[(DateTime, [Event])]]
          -- mapping with the full list of events is not very efficient, it would be
          -- better to remove the events from the list that have already been considered
          calWithEvents = map (mergeCal events) calWithoutEvents :: [[(DateTime, [Event])]]
      layout comp [T.pack $ show year, T.pack $ show month]
          [whamlet|
            <div .row>
                <div .col-sm-7>
                    <div #calendar>
                        $forall week <- calWithEvents
                            <div .row>
                                <div .col-sm-12 .col-md-12>
                                    $forall day <- week
                                        <div .day>#{dayno $ fst day}
                                            $forall event <- snd day
                                                <p>#{evTitle event}
                <div .col-sm-4>
                  <div .panel .panel-info>
                    <div .panel-heading>
                      <h3 .panel-title>Metadaten
                    <div .panel-body>
                      <dl>
                        <dt>Titel
                        <dd>
                        <dt>Beschreibung
                        <dd>
                        <dt>Startzeit
                        <dd>
                        <dt>Endezeit
                        <dd>
                  <div .panel .panel-danger>
                    <div .panel-heading>
                      <h3 .panel-title>Aktionen
                    <div .panel-body>
                      <a href=#addFolder role=button data-toggle=modal .btn .btn-default .btn-sm style="margin-bottom: 5px">Neuer Eintrag
                      <a href=#uploadFile role=button data-toggle=modal .btn .btn-default .btn-sm style="margin-bottom: 5px">Eintrag löschen
          |]

    provideRep $ renderEvents repo returnJson

  where renderEvents :: HasContentType a => Repository -> ([Event] -> Handler a) -> Handler a
        renderEvents repo as = do
              eitherEvents <- liftIO $ runEitherT $ getEventsForMonth repo year month
              case eitherEvents of
                  Left  e      -> do $logError $ T.pack $ show e
                                     as ([] :: [Event])
                  Right events -> as events

        getEvents :: Repository -> Handler [Event]
        getEvents repo = do
              eitherEvents <- liftIO $ runEitherT $ getEventsForMonth repo year month
              case eitherEvents of
                  Left  e      -> do $logError $ T.pack $ show e
                                     return ([] :: [Event])
                  Right events -> return events

        dayno :: DateTime -> Int
        dayno d = let (_, _, dno) = toGregorian' d in dno

        mergeCal :: [Event] -> [(DateTime, [Event])] -> [(DateTime, [Event])]
        mergeCal _ [] = []
        mergeCal events ((cal, _) : calRest) = (cal, eventsAt) : mergeCal eventsAfter calRest
          where
            (eventsAt, eventsAfter) = partition (\ev -> toGregorian' cal == toGregorian' (evStartDate ev)) events

eventForm :: Repository -> U.UUID -> Maybe Event -> Html -> MForm Handler (FormResult Event, Widget)
eventForm repo uuid mevent = do
  renderDivs $ makeEvent uuid repo
    <$> areq textField "Titel"        (                                fmap evTitle       mevent)
    <*> areq textField "Startdatum"   (     (T.pack . toSqlString) <$> fmap evStartDate   mevent)
    <*> aopt textField "Enddatum"     (fmap (T.pack . toSqlString) <$> fmap evEndDate     mevent)
    <*> areq textField "Beschreibung" (                                fmap evDescription mevent)
    <*> areq textField "Ort"          (                                fmap evLocation    mevent)
  where makeEvent :: U.UUID -> Repository -> T.Text -> T.Text -> Maybe T.Text -> T.Text -> T.Text -> Event
        makeEvent uuid repo title start end = Event uuid
                                                    repo
                                                    title
                                                    (fromMaybe startOfTime $ fromSqlString $ T.unpack     start)
                                                    (fromMaybe Nothing     $ fromSqlString . T.unpack <$> end)

getEventR :: T.Text -> Handler TypedContent
getEventR name = do
  comp <- component
  let repo = compRepository comp
  let name' = T.unpack name
  let uuid = read name'
  eitherEvent <- liftIO $ runEitherT $ readItem repo name'
  let mevent =  either (\e -> trace (show e) Nothing)
                       Just
                       eitherEvent
  selectRep $ do
    provideRep $ do
      (widget, encType) <- generateFormPost $ eventForm repo uuid mevent
      layout comp ["members", "event"] [whamlet|
        <form method=post action=@{EventR name} enctype=#{encType}>
          ^{widget}
          <button>Submit
      |]
    provideRep $ maybe notFound returnJson mevent

putEventR :: Handler ()
putEventR = do
  comp <- component
  let repo = compRepository comp
  req <- getRequest
  body <- liftIO $ strictRequestBody $ reqWaiRequest req
  let ev @ Event {..} = fromJust $ decode body
  ev' <- if U.null evUUID
         then liftIO $ newEvent repo evTitle evStartDate evEndDate evDescription evLocation
         else return ev { evRepo = repo }
  res <- liftIO $ runEitherT $ writeItem ev'
  either (fail . show) return res
  
postEventR :: T.Text -> Handler Html
postEventR name = do
  comp <- component
  let repo = compRepository comp
  let name' = T.unpack name
  let uuid = read name'
  ((result, widget), enctype) <- runFormPost $ eventForm repo uuid Nothing
  case result of
    FormSuccess event -> do 
      -- this should actually be removeItem (old path) >> writeItem (new path)
      result' <- liftIO $ runEitherT $ writeItem event
      case result' of
        Left e ->
          return $ trace (show e) ()
        _      ->
          return ()
      let (year, month, _) = toGregorian' $ evStartDate event
      redirect $ MemberCalendarMR (fromInteger year) month
    _                 ->
      layout comp ["members", "event"] [whamlet|
        <p>Da stimmt etwas nicht, versuch's nochmal
        <form method=post action=@{EventR name} enctype=#{enctype}>
            ^{widget}
            <button>Submit
      |]
 
auditTrail :: [T.Text] -> Widget
auditTrail path = do
  [whamlet|
    <ul .nav .nav-pills .navbar-nav>
      <li role=presentation .active>
        <a href=@{RootR}>Startseite
      <li role=presentation .active>
        <a href=@{MemberCalendarR}>Kalender
      ^{auditTrail' path}
  |]

-- TODO: simply build the trail using the path
auditTrail' :: [T.Text] -> Widget
auditTrail' [year, month] = do
  let year' = read $ T.unpack year :: Int
  let month' = read $ T.unpack month :: Int
  [whamlet|
    <li role=presentation .active>
      <a href=@{MemberCalendarMR year' month'} title=#{year}>#{year}
    <li role=presentation .active>
      <a href=@{MemberCalendarMR year' month'} title=#{month}>#{month}
  |]

auditTrail' path = do
  [whamlet|
    <li role=presentation .active>
      <a href=@{MemberCalendarR} title=#{T.concat path}>#{T.concat path}
  |]

naviChildren :: [T.Text] -> Maybe Widget
naviChildren _ = Nothing

-- calendarWidget :: Widget
-- calendarWidget = [whamlet|
--   <div .calendar .row>
--     ^{editCalItem}
-- |]
-- 
-- editCalItem :: Event -> Widget
-- editCalItem event = [whamlet|
--     <div #editCalItem .modal .fade>
--       <script>
--         function updateCalItem() {
--           \$.ajax(
--             { url: "@{EventR $ ContentPath $ evTitle event}/"
--             , type: "PUT"
--             , success: function(result) {
--               }
--             }
--           );
--         }
--       <div .modal-dialog>
--         <div .modal-content>
--           <div .modal-header>
--             <button type=button .close data-dismiss=modal aria-hidden=true>&times;
--             <h4 .modal-title>Neuer Ordner
--           <div .modal-body>
--             <label for=folderName>Name des neuen Ordners
--             <input #folderName type=text name=folderName>
--           <div .modal-footer>
--             <button type=button .btn .btn-default data-dismiss=modal>Schliessen
--             <button type=submit .btn .btn-primary onClick=editCalItem($('#folderName').val())>Erstellen
-- |]
