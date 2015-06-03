-- CBG
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.CBGWebSite
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import qualified CH.ComeBackGloebb.CBGWebSite.Repo.Impl.NewRepository as N

-- Yesod
import Yesod
import Yesod.Static
import Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import Control.Concurrent (newMVar)

main = do sem            <- newMVar True
          staticSettings <- static "static"
          manager        <- newManager conduitManagerSettings
          warp 8080 $
            CBGWebSite staticSettings sem manager
                (Repository "content")
                (Repository "members/list")
                (Repository "members/calendar")
