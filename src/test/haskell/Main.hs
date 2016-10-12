import           Test.Model.Asset
import           Test.Repo.Repository

main :: IO ()
main = do
  Test.Repo.Repository.main
  Test.Model.Asset.main
