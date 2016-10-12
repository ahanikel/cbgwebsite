module Test.Model.Asset where

import           CH.ComeBackGloebb.CBGWebSite.Model.Impl.Asset (Asset (..),
                                                                listAssets)
import           CH.ComeBackGloebb.CBGWebSite.Repo.Repository  (Persistent (..),
                                                                Repository (..),
                                                                RepositoryContext,
                                                                openRepository)
import           Control.Monad.Trans.Either                    (runEitherT)
import           System.Directory                              (removeDirectoryRecursive)
import           System.IO.Temp                                (createTempDirectory)
import           Test.Hspec

main :: IO ()
main = do
  tmpDir  <- createTempDirectory "/tmp" "assettest."
  let repo = openRepository tmpDir

  hspec $ do
    describe "Asset" $ do

      it "create a test asset repository" $ do
        result <- runEitherT $ createTestRepo repo
        result `shouldBe` Right repo

      it "list assets of the repository" $ do
        asset   <- runEitherT $ assetName . head <$> listAssets repo []
        asset   `shouldBe` Right "Verein"

        asset'  <- runEitherT $ assetName . head <$> listAssets repo ["Verein"]
        asset'  `shouldBe` Right "Statuten"

        asset'' <- runEitherT $ assetName . head <$> listAssets repo ["Verein", "Statuten"]
        asset'' `shouldBe` Right "Statuten.pdf"

  removeDirectoryRecursive tmpDir

createTestRepo :: Repository -> RepositoryContext Repository
createTestRepo repo = do
  let assets :: [Asset]
      assets = map (\a -> a { assetRepo = repo }) $ read "[Asset {assetRepo = Repository {root = \"data/assets\"}, assetPath = [\"Verein\"], assetName = \"Verein\", assetType = \"application/x-directory\", assetUploadedBy = \"nobody\", assetUploadedDate = 1970-01-01 00:00:00 UTC} ,Asset {assetRepo = Repository {root = \"data/assets\"}, assetPath = [\"Verein\",\"Statuten\"], assetName = \"Statuten\", assetType = \"application/x-directory\", assetUploadedBy = \"nobody\", assetUploadedDate = 1970-01-01 00:00:00 UTC} ,Asset {assetRepo = Repository {root = \"data/assets\"}, assetPath = [\"Verein\",\"Statuten\",\"Statuten.pdf\"], assetName = \"Statuten.pdf\", assetType = \"application/pdf\", assetUploadedBy = \"sigi\", assetUploadedDate = 2015-03-14 21:28:00 UTC}]"
  mapM_ writeItem assets
  return repo
