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

-- other imports
import           Control.Monad                                      (liftM)
import           Control.Monad.Trans.Either                         (runEitherT)
import           Data.DateTime
import           Data.Function                                      (on)
import           Data.List                                          (partition, sortBy, find)
import           Data.Maybe                                         (fromMaybe)
import qualified Data.Text as T
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
                <div .col-sm-10 .col-sm-offset-1>
                    <div #calendar>
                        $forall week <- calWithEvents
                            <div .row>
                                <div .col-sm-12 .col-md-12>
                                    $forall day <- week
                                        <div .day>#{dayno $ fst day}
                                            $forall event <- snd day
                                                <p>#{evTitle event}
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

        --eventId event = T.pack $ intercalate "/" [show year', show month', toSqlString $ evStartDate event, T.unpack $ evTitle event]
        --    where (year', month', _) = toGregorian' $ evStartDate event

        dayno :: DateTime -> Int
        dayno d = let (_, _, dno) = toGregorian' d in dno

        mergeCal :: [Event] -> [(DateTime, [Event])] -> [(DateTime, [Event])]
        mergeCal _ [] = []
        mergeCal events ((cal, _) : calRest) = (cal, eventsAt) : mergeCal eventsAfter calRest
          where
            (eventsAt, eventsAfter) = partition (\ev -> toGregorian' cal == toGregorian' (evStartDate ev)) events

eventForm :: Repository -> Maybe Event -> Html -> MForm Handler (FormResult Event, Widget)
eventForm repo mevent = do
  renderDivs $ makeEvent repo
    <$> areq textField "Titel"        (                                fmap evTitle       mevent)
    <*> areq textField "Startdatum"   (     (T.pack . toSqlString) <$> fmap evStartDate   mevent)
    <*> aopt textField "Enddatum"     (fmap (T.pack . toSqlString) <$> fmap evEndDate     mevent)
    <*> areq textField "Beschreibung" (                                fmap evDescription mevent)
    <*> areq textField "Ort"          (                                fmap evLocation    mevent)
  where makeEvent :: Repository -> T.Text -> T.Text -> Maybe T.Text -> T.Text -> T.Text -> Event
        makeEvent repo title start end = Event repo
                                               title
                                               (fromMaybe startOfTime $ fromSqlString $ T.unpack     start)
                                               (fromMaybe Nothing     $ fromSqlString . T.unpack <$> end)

getEventR :: T.Text -> Handler Html
getEventR name = do
  comp <- component
  let repo = compRepository comp
  eitherEvent <- liftIO $ runEitherT $ readItem repo (T.unpack name)
  let mevent =  either (\e -> trace (show e) Nothing)
                       Just
                       eitherEvent
  (widget, encType) <- generateFormPost $ eventForm repo mevent
  layout comp ["members", "event"] [whamlet|
     <form method=post action=@{EventR name} enctype=#{encType}>
         ^{widget}
         <button>Submit
  |]

postEventR :: T.Text -> Handler Html
postEventR name = do
  comp <- component
  let repo = compRepository comp
  ((result, widget), enctype) <- runFormPost $ eventForm repo Nothing
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

naviChildren :: [T.Text] -> Widget
naviChildren _ = return ()
