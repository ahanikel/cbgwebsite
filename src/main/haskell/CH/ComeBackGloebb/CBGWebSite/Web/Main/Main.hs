{-# LANGUAGE OverloadedStrings #-}

-- CBG
import qualified CH.ComeBackGloebb.CBGWebSite.Repo.Impl.FileStorage as N
import           CH.ComeBackGloebb.CBGWebSite.Repo.Repository
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.CBGWebSite

-- Yesod
import           Control.Concurrent                                 (newMVar)
import           Network.HTTP.Conduit                               (Manager,
                                                                     newManager,
                                                                     tlsManagerSettings)
import           Network.HTTP.Types                                 (status302)
import           Network.Wai                                        (isSecure,
                                                                     rawPathInfo,
                                                                     responseLBS)
import           Network.Wai.Handler.Warp                           (defaultSettings,
                                                                     setPort)
import           Network.Wai.Handler.WarpTLS                        (OnInsecure (..),
                                                                     onInsecure,
                                                                     runTLS,
                                                                     tlsSettings)
import           Yesod                                              (toWaiApp,
                                                                     warp)
import           Yesod.Static                                       (static)

-- other
import           Control.Monad.Logger                               (runStderrLoggingT)
import           Control.Monad.Trans                                (liftIO)
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Lazy                               as BL
import           Data.Text                                          (pack)
import           Database.Persist.Sqlite                            (runMigration,
                                                                     runSqlPool,
                                                                     withSqlitePool)

main = do sem                          <- newMVar True
          staticSettings               <- static "static"
          manager                      <- newManager tlsManagerSettings
          clientId                     <- readFile "google.clientId"
          clientSecret                 <- readFile "google.clientSecret"
          let warpSettings              = setPort 8080 defaultSettings
              warpTlsSettings           = (tlsSettings "server.crt" "server.key") { onInsecure = AllowInsecure }
              redirectHttp app req resp = if isSecure req
                                          then app req resp
                                          else resp $ responseLBS status302 [
                                            ("Location", B.concat ["https://test.comebackgloebb.ch", rawPathInfo req])
                                          ] BL.empty

          runStderrLoggingT $ withSqlitePool (pack "cbgusers.sqlite") 10 $ \pool -> do
            runSqlPool (runMigration migrateAll) pool
            liftIO $ do
              app <- toWaiApp $
                CBGWebSite staticSettings sem manager
                    (Repository "data/content")
                    (Repository "data/members")
                    (Repository "data/calendar")
                    (Repository "data/galleries")
                    (Repository "data/assets")
                    (pack clientId)
                    (pack clientSecret)
                    pool
              runTLS warpTlsSettings warpSettings (redirectHttp app)
