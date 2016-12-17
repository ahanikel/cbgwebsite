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

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.CBGWebSite (cbgWebSite) where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Root
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Assets as Assets
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Calendar as Calendar
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Content as Content
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.Galleries as Galleries
import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Handler.MemberList as MemberList
import CH.ComeBackGloebb.CBGWebSite.Web.Component

-- Yesod
import Yesod
import Yesod.Auth
import Yesod.Static
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Database.Persist.Sqlite (ConnectionPool, withSqlitePool, runSqlPool, runMigration)
import Control.Monad.Logger (runStderrLoggingT)
import Network.HTTP.Types (status302)
import Network.Wai (isSecure, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (OnInsecure(..), onInsecure, runTLS, tlsSettings)

-- other imports
import Control.Concurrent (newMVar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

mkYesodDispatch "CBGWebSite" resourcesCBGWebSite

mkFoundation :: ConnectionPool -> IO CBGWebSite
mkFoundation dbPool = do
  getStatic       <- static "static"
  getSem          <- newMVar True
  httpManager     <- newManager tlsManagerSettings
  let components   =
        [ Component "Content"
                    "Content"
                    Content.auditTrail
                    Content.naviChildren
                    (Repository "data/content")
        , Component "Members"
                    "Mitglieder"
                    MemberList.auditTrail
                    MemberList.naviChildren
                    (Repository "data/members")
        , Component "Calendar"
                    "Kalender"
                    Calendar.auditTrail
                    Calendar.naviChildren
                    (Repository "data/calendar")
        , Component "Galleries"
                    "Fotos"
                    Galleries.auditTrail
                    Galleries.naviChildren
                    (Repository "data/galleries")
        , Component "Assets"
                    "Dateien"
                    Assets.auditTrail
                    Assets.naviChildren
                    (Repository "data/assets")
        ]
  clientId        <- T.pack <$> readFile "google.clientId"
  clientSecret    <- T.pack <$> readFile "google.clientSecret"
  return CBGWebSite {..}

cbgWebSite :: IO ()
cbgWebSite = do
  let warpSettings = setPort 8080 defaultSettings
      warpTlsSettings = (tlsSettings "server.crt" "server.key")
                        { onInsecure = AllowInsecure }
  runStderrLoggingT $
    withSqlitePool (T.pack "cbgusers.sqlite") 10 $ \pool -> do
      runSqlPool (runMigration migrateAll) pool
      liftIO $ do
        foundation <- mkFoundation pool
        app <- toWaiApp foundation
        runTLS warpTlsSettings warpSettings (redirectHttp app)

redirectHttp :: Application -> Application
redirectHttp app req resp | isSecure req = app req resp

redirectHttp _   req resp                = resp $ responseLBS status302
                                           [ ("Location", B.concat
                                               [ "https://test.comebackgloebb.ch"
                                               , rawPathInfo req
                                               ]
                                             )
                                           ]
                                           BL.empty
