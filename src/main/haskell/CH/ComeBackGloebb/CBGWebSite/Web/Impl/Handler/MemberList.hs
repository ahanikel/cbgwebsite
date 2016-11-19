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

-- Yesod
import Yesod hiding (renderBootstrap)

-- other imports
import           Control.Monad                                      (liftM)
import           Control.Monad.Trans.Either                         (left,
                                                                     runEitherT)
import qualified Data.ByteString.Lazy.UTF8                          as UL8
import qualified Data.Text as T
import Debug.Trace

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

memberForm :: CBGWebSite -> Maybe Member -> Html -> MForm Handler (FormResult Member, Widget)
memberForm app mmember = do
  let repo = memberRepo app
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
getMemberR name = do app          <- getYesod
                     eitherMember <- liftIO $ runEitherT $ readItem (memberRepo app) (T.unpack name)
                     let mmember  =  either (\e -> trace (show e) Nothing)
                                            Just
                                            eitherMember
                     (widget, encType) <- generateFormPost $ memberForm app mmember
                     cbgLayout ["members", "list", "edit"] [whamlet|
                             <form .form-horizontal method=post action=@{MemberR name} enctype=#{encType}>
                                 ^{widget}
                                 <div .form-group>
                                     <button>Submit
                     |]

postMemberR :: T.Text -> Handler Html
postMemberR name = do
  app <- getYesod
  ((result, widget), enctype) <- runFormPost $ memberForm app Nothing
  case result of FormSuccess member -> do result' <- liftIO $ runEitherT $ writeItem member
                                          case result' of
                                              Left e -> return $ trace (show e) ()
                                              _      -> return ()
                                          redirect MemberListR
                 _                  -> cbgLayout ["members", "list", "edit"] [whamlet|
                                           <p>Da stimmt etwas nicht, versuch's nochmal
                                           <form method=post action=@{MemberR name} enctype=#{enctype}>
                                               ^{widget}
                                               <button>Submit
                                       |]

getMemberListR :: Handler TypedContent
getMemberListR = selectRep $ do
    provideRep $ renderMembers $ \members ->
                cbgLayout ["members", "list"] [whamlet|
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
              app           <- getYesod
              eitherMembers <- liftIO $ runEitherT $ getMemberList (memberRepo app)
              case eitherMembers of
                  Left  e       -> do $logError $ T.pack $ show e
                                      as ([] :: [Member])
                  Right members -> as members
