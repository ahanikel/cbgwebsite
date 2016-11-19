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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Content where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Layout

-- Yesod
import Yesod

-- other imports
import           Control.Monad                                      (liftM)
import           Control.Monad.Trans.Either                         (left,
                                                                     runEitherT)
import qualified Data.ByteString.Lazy.UTF8                          as UL8
import qualified Data.Text as T

getContentR :: ContentPath -> Handler Html
getContentR (ContentPath pieces) = cbgLayout ("content" : pieces) $ do
    app <- getYesod
    eitherNode <- liftIO $ runEitherT $ do
        let url = map T.unpack pieces
        getNode (contentRepo app) url
    case eitherNode of
        Left _ -> notFound
        Right node -> do
          res <- liftIO $ runEitherT $ liftM UL8.toString $ getProperty node "text.html"
          prop <- either (fail.show) return res
          toWidget $ preEscapedToMarkup prop

getEditContentR :: ContentPath -> Handler Html
getEditContentR (ContentPath pieces) = do
    let url = map T.unpack pieces
    app <- getYesod
    eitherNode <- liftIO $ runEitherT $ getNode (contentRepo app) url
    case eitherNode of
        Left _ -> case url of
          [] -> notFound
          _  -> do
            editLayout ("content" : pieces) $ toWidget $ toHtml T.empty
        Right node -> do
          res <- liftIO $ runEitherT $ liftM UL8.toString $ getProperty node "text.html"
          prop <- either (fail . show) return res
          let contentBody = toWidget $ toHtml prop
          -- let propertiesWidget =
          -- let permissionsWidget =
          editLayout ("content" : pieces) contentBody

postEditContentR :: ContentPath -> Handler Html
postEditContentR (ContentPath pieces) = do
    let url = map T.unpack pieces
    app <- getYesod
    eitherNode <- liftIO $ runEitherT $ getNode (contentRepo app) url
    node <- case eitherNode of
        Left _ -> case url of
          [] -> notFound
          _  -> do
            let name = last url
                repo = contentRepo app
                node = Node name url [] repo
            liftIO $ runEitherT $ writeNode node
            return node
        Right node -> return node
    body <- runInputPost $ ireq textField "body"
    liftIO $ runEitherT $ writeProperty node "text.html" (UL8.fromString $ T.unpack body)
    withUrlRenderer [hamlet||]
