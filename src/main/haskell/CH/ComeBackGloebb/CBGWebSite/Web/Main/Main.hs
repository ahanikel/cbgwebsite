-- CBG
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.CBGWebSite
import           CH.ComeBackGloebb.CBGWebSite.Repo.Repository
import qualified CH.ComeBackGloebb.CBGWebSite.Repo.Impl.FileStorage as N

-- Yesod
import Yesod
import Yesod.Static
import Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import Control.Concurrent (newMVar)

import Data.Text (pack)

main = do sem            <- newMVar True
          staticSettings <- static "static"
          manager        <- newManager conduitManagerSettings
          clientId       <- readFile "google.clientId"
          clientSecret   <- readFile "google.clientSecret"
          warp 8080 $
            CBGWebSite staticSettings sem manager
                (Repository "content")
                (Repository "data/members")
                (Repository "data/calendar")
                (pack clientId)
                (pack clientSecret)
