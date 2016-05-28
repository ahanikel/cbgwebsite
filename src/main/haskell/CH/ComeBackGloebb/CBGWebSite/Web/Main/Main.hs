-- CBG
import qualified CH.ComeBackGloebb.CBGWebSite.Repo.Impl.FileStorage as N
import           CH.ComeBackGloebb.CBGWebSite.Repo.Repository
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.CBGWebSite

-- Yesod
import           Control.Concurrent                                 (newMVar)
import           Network.HTTP.Conduit                               (Manager, conduitManagerSettings,
                                                                     newManager)
import           Network.Wai.Handler.Warp                           (defaultSettings,
                                                                     setPort)
import           Network.Wai.Handler.WarpTLS                        (runTLS,
                                                                     tlsSettings)
import           Yesod                                              (toWaiApp,
                                                                     warp)
import           Yesod.Static                                       (static)

import           Data.Text                                          (pack)

main = do sem                <- newMVar True
          staticSettings     <- static "static"
          manager            <- newManager conduitManagerSettings
          clientId           <- readFile "google.clientId"
          clientSecret       <- readFile "google.clientSecret"
          let warpSettings    = setPort 8080 defaultSettings
              warpTlsSettings = tlsSettings "server.crt" "server.key"

          app <- toWaiApp $
            CBGWebSite staticSettings sem manager
                (Repository "data/content")
                (Repository "data/members")
                (Repository "data/calendar")
                (Repository "data/galleries")
                (pack clientId)
                (pack clientSecret)

          runTLS warpTlsSettings warpSettings app
