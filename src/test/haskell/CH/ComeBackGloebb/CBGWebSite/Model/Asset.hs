import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Asset (Asset (..),
                                                                listAssets)
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Utils  (check)
import           CH.ComeBackGloebb.CBGWebSite.Repo.Repository  (Persistent (..),
                                                                Repository (..),
                                                                RepositoryContext,
                                                                openRepository)
import           Control.Monad.Trans.Either                    (runEitherT)
import           System.Directory                              (removeDirectoryRecursive)
import           System.IO.Temp                                (createTempDirectory)
import           Test.Hspec

main = do

  result <- runEitherT $ createTestRepo >>= testListAssets >>= removeTestRepo

  case result of
    Left e -> fail $ show e
    Right _ -> print "ok!"

createTestRepo :: RepositoryContext Repository
createTestRepo = do
  tmpDir  <- check $ createTempDirectory "/tmp" "assettest."
  let repo = openRepository tmpDir
      assets :: [Asset]
      assets = map (\a -> a { assetRepo = repo }) $ read "[Asset {assetRepo = Repository {root = \"data/assets\"}, assetPath = [\"Verein\"], assetName = \"Verein\", assetType = \"application/x-directory\", assetUploadedBy = \"nobody\", assetUploadedDate = 1970-01-01 00:00:00 UTC} ,Asset {assetRepo = Repository {root = \"data/assets\"}, assetPath = [\"Verein\",\"Statuten\"], assetName = \"Statuten\", assetType = \"application/x-directory\", assetUploadedBy = \"nobody\", assetUploadedDate = 1970-01-01 00:00:00 UTC} ,Asset {assetRepo = Repository {root = \"data/assets\"}, assetPath = [\"Verein\",\"Statuten\",\"Statuten.pdf\"], assetName = \"Statuten.pdf\", assetType = \"application/pdf\", assetUploadedBy = \"sigi\", assetUploadedDate = 2015-03-14 21:28:00 UTC}]"
  mapM_ writeItem assets
  return repo

testListAssets :: Repository -> RepositoryContext Repository
testListAssets repo = do
  let path = []
  assets <- listAssets repo path
  check $ mapM_ print assets
  let path = assetPath $ head assets
  assets <- listAssets repo path
  check $ mapM_ print assets
  let path = assetPath $ head assets
  assets <- listAssets repo path
  check $ mapM_ print assets
  return repo

removeTestRepo :: Repository -> RepositoryContext ()
removeTestRepo repo = check $ removeDirectoryRecursive $ root repo
