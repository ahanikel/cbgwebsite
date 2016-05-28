-- CBG
import qualified CH.ComeBackGloebb.CBGWebSite.Repo.Impl.FileStorage as N
import           CH.ComeBackGloebb.CBGWebSite.Repo.Repository
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.CBGWebSite

-- Yesod
import           Control.Concurrent                                 (newMVar)
import           Network.HTTP.Conduit                               (Manager, conduitManagerSettings,
                                                                     newManager)
import           Yesod
import           Yesod.Static

import           Data.Text                                          (pack)

main = do sem            <- newMVar True
          staticSettings <- static "static"
          manager        <- newManager conduitManagerSettings
          clientId       <- readFile "google.clientId"
          clientSecret   <- readFile "google.clientSecret"
          warp 8080 $
            CBGWebSite staticSettings sem manager
                (Repository "data/content")
                (Repository "data/members")
                (Repository "data/calendar")
                (Repository "data/galleries")
                (pack clientId)
                (pack clientSecret)
