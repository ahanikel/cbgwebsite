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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.MemberList where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Model.Impl.Member
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Layout
import CH.ComeBackGloebb.CBGWebSite.Web.Component

-- Yesod
import Yesod hiding (renderBootstrap)
import Yesod.Auth
import Text.Hamlet

-- other imports
import           Control.Monad.Trans.Either                         (runEitherT)
import qualified Data.Text as T
import           Data.List (find)
import           Debug.Trace

component :: Handler (Component CBGWebSite)
component = do
  cs <- components <$> getYesod
  let mc = find (== "Members") cs
  case mc of
    Nothing -> fail "Component not found."
    Just c  -> return c

renderBootstrap aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
        widget = [whamlet|
                        \#{fragment}
                        $forall view <- views
                            <div .form-group :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
                                <label .col-md-2 .control-label for=#{fvId view}>#{fvLabel view}
                                <div .col-md-10>
                                    ^{fvInput view}
                 |]
    return (res, widget)

memberForm :: Repository -> Maybe Member -> Html -> MForm Handler (FormResult Member, Widget)
memberForm repo mmember = do
  renderBootstrap $ Member repo
    <$> areq textField (fieldDef "Vorname") (memFirstname <$> mmember)
    <*> areq textField (fieldDef "Name")    (memName      <$> mmember)
    <*> areq textField (fieldDef "Strasse") (memAddress   <$> mmember)
    <*> areq textField (fieldDef "Ort")     (memLocality  <$> mmember)
    <*> areq textField (fieldDef "Status")  (memStatus    <$> mmember)
    <*> areq textField (fieldDef "Telefon") (memPhone     <$> mmember)
    <*> areq textField (fieldDef "Mobile")  (memMobile    <$> mmember)
    <*> areq textField (fieldDef "E-Mail")  (memEmail     <$> mmember)
  where fieldDef name = name { fsAttrs = [("class", "form-control")] }

getMemberR :: T.Text -> Handler Html
getMemberR name = do
  comp         <- component
  let repo     =  compRepository comp
  eitherMember <- liftIO $ runEitherT $ readItem repo (T.unpack name)
  let mmember  =  either (\e -> trace (show e) Nothing)
                  Just
                  eitherMember
  (widget, encType) <- generateFormPost $ memberForm repo mmember
  layout comp ["members", "list", "edit"] [whamlet|
    <form .form-horizontal method=post action=@{MemberR name} enctype=#{encType}>
      ^{widget}
      <div .form-group>
        <button>Submit
  |]

postMemberR :: T.Text -> Handler Html
postMemberR name = do
  comp <- component
  let repo = compRepository comp
  ((result, widget), enctype) <- runFormPost $ memberForm repo Nothing
  case result of
    FormSuccess member -> do
      result' <- liftIO $ runEitherT $ writeItem member
      case result' of
        Left e -> return $ trace (show e) ()
        _      -> return ()
      redirect MemberListR
    _ ->
      layout comp ["members", "list", "edit"] [whamlet|
        <p>Da stimmt etwas nicht, versuch's nochmal
        <form method=post action=@{MemberR name} enctype=#{enctype}>
          ^{widget}
          <button>Submit
      |]

getMemberListR :: Handler TypedContent
getMemberListR = selectRep $ do
  provideRep $ renderMembers $ \members -> do
    comp <- component
    layout comp ["members", "list"] [whamlet|
      <table>
        <tr>
          <th>Vorname
          <th>Name
          <th>Strasse
          <th>Ort
          <th>Status
          <th>Telefon
          <th>Mobile
          <th>E-Mail
        $forall member <- members
          <tr>
            <td>#{memFirstname member}
            <td>#{memName      member}
            <td>#{memAddress   member}
            <td>#{memLocality  member}
            <td>#{memStatus    member}
            <td>#{memPhone     member}
            <td>#{memMobile    member}
            <td>#{memEmail     member}
            <td>
              <a href=@{MemberR $ memberId member}>
                <button>Edit
    |]
  provideRep $ renderMembers returnJson
  where renderMembers :: HasContentType a => ([Member] -> Handler a) -> Handler a
        renderMembers as = do
          repo          <- compRepository <$> component
          eitherMembers <- liftIO $ runEitherT $ getMemberList repo
          case eitherMembers of
            Left  e       -> do
              $logError $ T.pack $ show e
              as ([] :: [Member])
            Right members -> as members
 
auditTrail :: [T.Text] -> Widget
auditTrail _ = do
  [whamlet|
    <ul .nav .nav-pills .navbar-nav>
      <li role=presentation .active>
        <a href=@{RootR}>Startseite
      <li role=presentation .active>
        <a href=@{MemberListR}>Mitgliederliste
  |]

naviChildren :: [T.Text] -> Widget
naviChildren _ = return ()
