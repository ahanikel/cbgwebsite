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
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Privileges
-- Yesod
import Yesod

-- other imports
import           Control.Monad                                      (liftM)
import           Control.Monad.Trans.Either                         (left,
                                                                     runEitherT)
import qualified Data.ByteString.Lazy.UTF8                          as UL8
import           Data.DateTime
import           Data.Function                                      (on)
import           Data.List (intercalate, partition, sortBy)
import           Data.Maybe                                         (fromMaybe)
import qualified Data.Text as T
import           Debug.Trace

getMemberCalendarR :: Handler ()
getMemberCalendarR = do
    (year, month, _) <- liftIO $ liftM toGregorian' getCurrentTime
    redirect $ MemberCalendarMR (fromInteger year) month

getMemberCalendarMR :: Int -> Int -> Handler TypedContent
getMemberCalendarMR year month = if   month < 1 || month > 12
                                 then notFound
                                 else selectRep $ do

    provideRep $ do events              <- sortBy (compare `on` evStartDate) <$> getEvents
                    let cal              = calendarForMonth year month         :: [[DateTime]]
                        calWithoutEvents = map (map (\d -> (d, []))) cal       :: [[(DateTime, [Event])]]
                        -- mapping with the full list of events is not very efficient, it would be
                        -- better to remove the events from the list that have already been considered
                        calWithEvents = map (mergeCal events) calWithoutEvents :: [[(DateTime, [Event])]]
                    cbgLayout ["members", "calendar"]
                        [whamlet|
                            <div .container>
                                <div .row>
                                    <div .col-md-8 .col-md-offset-1>
                                        <div #calendar .container>
                                            $forall week <- calWithEvents
                                                <div .row>
                                                    $forall day <- week
                                                        <div .col-md-1>
                                                            <div .thumbnail>#{dayno $ fst day}
                                                                $forall event <- snd day
                                                                    <p>#{evTitle event}
                        |]

    provideRep $ renderEvents returnJson

  where renderEvents :: HasContentType a => ([Event] -> Handler a) -> Handler a
        renderEvents as = do
              app          <- getYesod
              eitherEvents <- liftIO $ runEitherT $ getEventsForMonth (calendarRepo app) year month
              case eitherEvents of
                  Left  e      -> do $logError $ T.pack $ show e
                                     as ([] :: [Event])
                  Right events -> as events

        getEvents :: Handler [Event]
        getEvents = do
              app          <- getYesod
              eitherEvents <- liftIO $ runEitherT $ getEventsForMonth (calendarRepo app) year month
              case eitherEvents of
                  Left  e      -> do $logError $ T.pack $ show e
                                     return ([] :: [Event])
                  Right events -> return events

        eventId event = T.pack $ intercalate "/" [show year', show month', toSqlString $ evStartDate event, T.unpack $ evTitle event]
            where (year', month', _) = toGregorian' $ evStartDate event

        dayno :: DateTime -> Int
        dayno d = let (_, _, dno) = toGregorian' d in dno

        mergeCal :: [Event] -> [(DateTime, [Event])] -> [(DateTime, [Event])]
        mergeCal _ [] = []
        mergeCal events ((cal, _) : calRest) = (cal, eventsAt) : mergeCal eventsAfter calRest
          where
            (eventsAt, eventsAfter) = partition (\ev -> toGregorian' cal == toGregorian' (evStartDate ev)) events

eventForm :: CBGWebSite -> Maybe Event -> Html -> MForm Handler (FormResult Event, Widget)
eventForm app mevent = do
  renderDivs $ makeEvent (contentRepo app)
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
getEventR name = do app               <- getYesod
                    eitherEvent       <- liftIO $ runEitherT $ readItem (calendarRepo app) (T.unpack name)
                    let mevent        =  either (\e -> trace (show e) Nothing)
                                                Just
                                                eitherEvent
                    (widget, encType) <- generateFormPost $ eventForm app mevent
                    cbgLayout ["members", "event"] [whamlet|
                       <form method=post action=@{EventR name} enctype=#{encType}>
                           ^{widget}
                           <button>Submit
                    |]

postEventR :: T.Text -> Handler Html
postEventR name = do
  app <- getYesod
  ((result, widget), enctype) <- runFormPost $ eventForm app Nothing
  case result of FormSuccess event -> do app               <- getYesod
                                         let strName       =  T.unpack name
                                         -- this should actually be removeItem (old path) >> writeItem (new path)
                                         result' <- liftIO $ runEitherT $ writeItem event
                                         case result' of
                                             Left e -> return $ trace (show e) ()
                                             _      -> return ()
                                         let (year, month, _) = toGregorian' $ evStartDate event
                                         redirect $ MemberCalendarMR (fromInteger year) month
                 _                 -> cbgLayout ["members", "event"] [whamlet|
                                          <p>Da stimmt etwas nicht, versuch's nochmal
                                          <form method=post action=@{EventR name} enctype=#{enctype}>
                                              ^{widget}
                                              <button>Submit
                                      |]
