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
import Text.Cassius
import Text.Julius
import Network.Wai                                                  (strictRequestBody)

-- other imports
import           Control.Monad                                      (liftM)
import           Control.Monad.Trans.Either                         (runEitherT)
import           Data.Aeson                                         (eitherDecode, encode)
import           Data.DateTime
import           Data.Either                                        (either)
import           Data.Function                                      (on)
import           Data.List                                          (partition, sortBy, find)
import           Data.Maybe                                         (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
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
  events <- sortBy (compare `on` evStartDate) <$> getEvents repo
  if month < 1 || month > 12
  then notFound
  else selectRep $ do
    provideRep $ do
      layout comp [T.pack $ show year, T.pack $ show month] $ do
          [whamlet|
            <div .row ng-app=calendarApp ng-controller=CalendarController>
              <div .col-sm-7>
                <div #calendar>
                  <div .row ng-repeat="week in events">
                    <div .col-sm-12 .col-md-12>
                      <div ng-repeat="day in week" .day>{{day[0]}}
                        <a ng-repeat="event in day[1]" .event-summary ng-click="setCurrent(event)">
                          <p>{{event.evTitle}}
              <div .col-sm-4>
                <div .panel .panel-info>
                  <div .panel-heading>
                    <h3 .panel-title>Event-Details
                  <div .panel-body>
                    <form #event-form .form-horizontal>
                      <div #evUUID-field .form-group>
                        <input #evUUID-input .form-control type=hidden name=evUUID ng-model=current.evUUID>
                      <div #evTitle-field .form-group>
                        <label #evTitle-label for=evTitle-input>Titel
                        <input #evTitle-input .form-control type=text name=evTitle ng-model=current.evTitle>
                      <div #evStartDate-field .form-group>
                        <label #evStartDate-label for=evStartDate-input>Startdatum
                        <input #evStartDate-input .form-control type=text name=evStartDate ng-model=current.evStartDate>
                      <div #evEndDate-field .form-group>
                        <label #evEndDate-label for=evEndDate-input>Enddatum
                        <input #evEndDate-input .form-control type=text name=evEndDate ng-model=current.evEndDate>
                      <div #evDescription-field .form-group>
                        <label #evDescription-label for=evDescription-input>Beschreibung
                        <input #evDescription-input .form-control type=text name=evDescription ng-model=current.evDescription>
                      <div #evLocation-field .form-group>
                        <label #evLocation-label for=evLocation-input>Ort
                        <input #evLocation-input .form-control type=text name=evLocation ng-model=current.evLocation>
                      <div #evButtons .form-group>
                        <button #event-submit-button ng-click=submitEdit()>OK
                        <button #event-cancel-button ng-click=cancelEdit()>Abbrechen
                        <button #event-edit-button ng-click=beginEdit()>Bearbeiten
                <div .panel .panel-danger>
                  <div .panel-heading>
                    <h3 .panel-title>Aktionen
                  <div .panel-body>
                    <a ng-click=newEvent() role=button .btn .btn-primary .btn-sm style="margin-bottom: 5px">Neuer Eintrag
                    <a ng-click=deleteEvent() role=button .btn .btn-primary .btn-sm style="margin-bottom: 5px">Eintrag löschen
          |]
          toWidget [julius|
            $.datepicker.setDefaults(
            { "dateFormat": "yy-mm-dd'T'"
            , "showButtonPanel": false
            });
            $.timepicker.setDefaults(
            { "timeFormat": "HH:mm:'00'"
            , "separator": ""
            });
            $('#evStartDate-input').datetimepicker();
            $('#evEndDate-input').datetimepicker();
            angular.module('calendarApp', [])
            .controller('CalendarController', function ($http, $scope) {
              var self = this;
              eventView();
              eventClear();
              $http.get('@{MemberCalendarMR year month}')
              .then(function (response) {
                $scope.events = response.data;
                eventHide();
                eventView();
              });
              function eventClear() {
                $("#event-form input, #event-form textarea").val(null);
                $scope.current = { "evUUID" : ""
                                 , "evTitle" : ""
                                 , "evStartDate" : ""
                                 , "evEndDate" : ""
                                 , "evDescription" : ""
                                 , "evLocation" : ""
                                 };
                $scope.original = angular.copy($scope.current);
              }
              function eventHide() {
                $("#event-form").css("visibility", "hidden");
                $("#event-edit-button").css("visibility", "hidden");
                $("#event-submit-button").css("visibility", "hidden");
                $("#event-cancel-button").css("visibility", "hidden");
                $("#event-form input").prop("readonly", true).prop("disabled", true);
              }
              function eventView() {
                $("#event-form").removeClass("event-edit");
                $("#event-form").addClass("event-selected");
                $("#event-form").css("visibility", "visible");
                $("#event-edit-button").css("visibility", "visible");
                $("#event-submit-button").css("visibility", "collapse");
                $("#event-cancel-button").css("visibility", "collapse");
                $("#event-form input").prop("readonly", true).prop("disabled", true);
              }
              function eventEdit() {
                $("#event-form").addClass("event-edit");
                $("#event-edit-button").css("visibility", "collapse");
                $("#event-submit-button").css("visibility", "visible");
                $("#event-cancel-button").css("visibility", "visible");
                $("#event-form input").prop("readonly", false).prop("disabled", false);
              }
              var url = '@{EventR ""}';
              $scope.eventHide = eventHide;
              $scope.cancelEdit = function() {
                angular.copy($scope.original, $scope.current);
                eventView();
              };
              $scope.beginEdit = function() {
                eventEdit();
              };
              $scope.submitEdit = function() {
                if ($scope.current.evUUID == "") {
                  $scope.current.evUUID = "00000000-0000-0000-0000-000000000000";
                }
                $http.post(url.substring(0, url.length - 1) + $scope.current.evUUID, $scope.current)
                .then(function(response) {
                  if ($scope.current.evUUID == "00000000-0000-0000-0000-000000000000") {
                    $http.get('@{MemberCalendarMR year month}')
                    .then(function (response) {
                      $scope.events = response.data;
                      eventClear();
                    });
                  }
                  eventView();
                });
              };
              $scope.newEvent = function() {
                eventClear();
                eventView();
                eventEdit();
              };
              $scope.deleteEvent = function() {
                $http.delete(url.substring(0, url.length - 1) + $scope.current.evUUID)
                .then(function(response) {
                  $http.get('@{MemberCalendarMR year month}')
                  .then(function (response) {
                    $scope.events = response.data;
                    eventClear();
                    eventView();
                  });
                });
              };
              $scope.setCurrent = function(event) {
                $scope.current = event;
                $scope.original = angular.copy(event);
                eventView();
              };
            });
          |]

    provideRep $ returnJson $ calWithEvents events

  where getEvents :: Repository -> Handler [Event]
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

        cal              = calendarForMonth year month         :: [[DateTime]]
        calWithoutEvents = map (map (\d -> (d, []))) cal       :: [[(DateTime, [Event])]]

        -- mapping with the full list of events is not very efficient, it would be
        -- better to remove the events from the list that have already been considered
        calWithEvents' :: [Event] -> [[(DateTime, [Event])]]
        calWithEvents' events = map (mergeCal events) calWithoutEvents :: [[(DateTime, [Event])]]
        calWithEvents :: [Event] -> [[(Int, [Event])]]
        calWithEvents = map (map (\(dt, es) -> (dayno dt, es))) . calWithEvents'


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

postEventR :: T.Text -> Handler TypedContent
postEventR name = do
  comp <- component
  let repo = compRepository comp
  let name' = T.unpack name
  let uuid = read name' :: U.UUID
  selectRep $ do
    provideRep $ do
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
    provideRep $ do
      req <- getRequest
      body <- liftIO $ strictRequestBody $ reqWaiRequest req
      let ev @ Event {..} = either error id $ eitherDecode body
      ev' <- if U.null evUUID
             then liftIO $ newEvent repo evTitle evStartDate evEndDate evDescription evLocation
             else return ev { evRepo = repo }
      res <- liftIO $ runEitherT $ writeItem ev'
      either (fail . show) returnJson res

deleteEventR :: T.Text -> Handler TypedContent
deleteEventR name = do
  comp <- component
  let repo = compRepository comp
  let name' = T.unpack name
  let uuid = read name' :: U.UUID
  selectRep $ do
    provideRep $ do
      res <- liftIO $ runEitherT $ do
        ev <- readItem repo (show uuid) :: RepositoryContext Event
        deleteItem ev
      either (fail . show) returnJson res

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

getMemberCalendarListR :: Handler TypedContent
getMemberCalendarListR = do
  comp <- component
  let repo = compRepository comp
  events <- sortBy (compare `on` evStartDate) <$> getEvents repo
  selectRep $ do
    provideRep $ do
      layout comp ["list"] $ do
        [whamlet|
          <div .row ng-app=calendarApp ng-controller=CalendarController>
            <div .col-sm-7>
              <div .panel .panel-success>
                <div ng-click=eventHide() .panel-heading>
                  <h3 .panel-title>Kalender als Liste
                <div .panel-body>
                  <table>
                    <thead>
                      <tr>
                        <th>Wann?
                        <th>Was?
                    <tbody>
                      <tr ng-repeat="event in events" ng-click="setCurrent(event)">
                        <td>{{event.evStartDate}}
                        <td>{{event.evTitle}}
            <div .col-sm-4>
              <div .panel .panel-info>
                <div .panel-heading>
                  <h3 .panel-title>Event-Details
                <div .panel-body>
                  <form #event-form .form-horizontal>
                    <div #evUUID-field .form-group>
                      <label #evUUID-label for=evUUID-input>UUID
                      <input #evUUID-input .form-control type=text name=evUUID ng-model=current.evUUID>
                    <div #evTitle-field .form-group>
                      <label #evTitle-label for=evTitle-input>Titel
                      <input #evTitle-input .form-control type=text name=evTitle ng-model=current.evTitle>
                    <div #evStartDate-field .form-group>
                      <label #evStartDate-label for=evStartDate-input>Startdatum
                      <input #evStartDate-input .form-control type=text name=evStartDate ng-model=current.evStartDate>
                    <div #evEndDate-field .form-group>
                      <label #evEndDate-label for=evEndDate-input>Enddatum
                      <input #evEndDate-input .form-control type=text name=evEndDate ng-model=current.evEndDate>
                    <div #evDescription-field .form-group>
                      <label #evDescription-label for=evDescription-input>Beschreibung
                      <input #evDescription-input .form-control type=text name=evDescription ng-model=current.evDescription>
                    <div #evLocation-field .form-group>
                      <label #evLocation-label for=evLocation-input>Ort
                      <input #evLocation-input .form-control type=text name=evLocation ng-model=current.evLocation>
                    <div #evButtons .form-group>
                      <button #event-submit-button ng-click=submitEdit()>OK
                      <button #event-cancel-button ng-click=cancelEdit()>Abbrechen
                      <button #event-edit-button ng-click=beginEdit()>Bearbeiten
              <div .panel .panel-danger>
                <div .panel-heading>
                  <h3 .panel-title>Aktionen
                <div .panel-body>
                  <a ng-click=newEvent() role=button .btn .btn-primary .btn-sm style="margin-bottom: 5px">Neuer Eintrag
                  <a ng-click=deleteEvent() role=button .btn .btn-primary .btn-sm style="margin-bottom: 5px">Eintrag löschen
        |]
        toWidget [julius|
          angular.module('calendarApp', [])
          .controller('CalendarController', function ($http, $scope) {
            var self = this;
            eventView();
            eventClear();
            $http.get('@{MemberCalendarListR}')
            .then(function (response) {
              $scope.events = response.data;
              eventHide();
              eventView();
            });
            function eventClear() {
              $("#event-form input, #event-form textarea").val(null);
              $scope.current = { "evUUID" : ""
                               , "evTitle" : ""
                               , "evStartDate" : ""
                               , "evEndDate" : ""
                               , "evDescription" : ""
                               , "evLocation" : ""
                               };
              $scope.original = angular.copy($scope.current);
            }
            function eventHide() {
              $("#event-form").css("visibility", "hidden");
              $("#event-edit-button").css("visibility", "hidden");
              $("#event-submit-button").css("visibility", "hidden");
              $("#event-cancel-button").css("visibility", "hidden");
              $("#event-form input").prop("readonly", true).prop("disabled", true);
            }
            function eventView() {
              $("#event-form").removeClass("event-edit");
              $("#event-form").addClass("event-selected");
              $("#event-form").css("visibility", "visible");
              $("#event-edit-button").css("visibility", "visible");
              $("#event-submit-button").css("visibility", "collapse");
              $("#event-cancel-button").css("visibility", "collapse");
              $("#event-form input").prop("readonly", true).prop("disabled", true);
            }
            function eventEdit() {
              $("#event-form").addClass("event-edit");
              $("#event-edit-button").css("visibility", "collapse");
              $("#event-submit-button").css("visibility", "visible");
              $("#event-cancel-button").css("visibility", "visible");
              $("#event-form input").prop("readonly", false).prop("disabled", false);
            }
            var url = '@{EventR ""}';
            $scope.eventHide = eventHide;
            $scope.cancelEdit = function() {
              angular.copy($scope.original, $scope.current);
              eventView();
            };
            $scope.beginEdit = function() {
              eventEdit();
            };
            $scope.submitEdit = function() {
              if ($scope.current.evUUID == "") {
                $scope.current.evUUID = "00000000-0000-0000-0000-000000000000";
              }
              $http.post(url.substring(0, url.length - 1) + $scope.current.evUUID, $scope.current);
              if ($scope.current.evUUID == "00000000-0000-0000-0000-000000000000") {
                $http.get('@{MemberCalendarListR}')
                .then(function (response) {
                  $scope.events = response.data;
                  eventClear();
                  eventHide();
                });
              }
              eventView();
            };
            $scope.newEvent = function() {
              eventClear();
              eventEdit();
            };
            $scope.deleteEvent = function() {
              $http.delete(url.substring(0, url.length - 1) + $scope.current.evUUID);
              $http.get('@{MemberCalendarListR}')
              .then(function (response) {
                $scope.events = response.data;
                eventClear();
                eventHide();
              });
            };
            $scope.setCurrent = function(event) {
              $scope.current = event;
              $scope.original = angular.copy(event);
              eventView();
            };
          })
        |]
    provideRep $ returnJson events
  where getEvents :: Repository -> Handler [Event]
        getEvents repo = do
              eitherEvents <- liftIO $ runEitherT $ getAllEvents repo
              case eitherEvents of
                  Left  e      -> do $logError $ T.pack $ show e
                                     return ([] :: [Event])
                  Right events -> return events
