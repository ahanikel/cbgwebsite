-- CBG
import CBGWebSite

-- Yesod
import Yesod
import Yesod.Static
import Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import Control.Concurrent (newMVar)

main = do sem            <- newMVar True
          staticSettings <- static "static"
          manager        <- newManager conduitManagerSettings
          warp 8080 $ CBGWebSite staticSettings sem manager