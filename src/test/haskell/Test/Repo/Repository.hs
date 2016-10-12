module Test.Repo.Repository where

import           System.Directory (removeDirectoryRecursive)
import           System.IO.Temp   (createTempDirectory)
import           Test.Hspec

withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action = do
  tmpDir <- createTempDirectory "/tmp" "repotest."
  action tmpDir
  removeDirectoryRecursive tmpDir

withRepo :: (Repository -> IO ()) -> IO ()
withRepo action = withTempDir $ \tmpDir -> do
  action $ openRepository tmpDir

testRepository :: IO ()
testRepository = withRepo $ \repo -> do
  hspec $ do
    describe "Repository" $ do
      it "write node" $ do
        let node = Node "testnode" [] [] repo
        res <- runEitherT $ writeNode node
        res `shouldBe` Right ()
      it "write property" $ do
        let node = Node "testnode" [] [] repo
        
