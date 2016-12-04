{-# LANGUAGE OverloadedStrings #-}

-- CBG
import qualified CH.ComeBackGloebb.CBGWebSite.Repo.Impl.FileStorage as N
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.CBGWebSite

--import System.Remote.Monitoring

main :: IO ()
main = do
  --forkServer "localhost" 8000 -- monitoring server
  cbgWebSite
