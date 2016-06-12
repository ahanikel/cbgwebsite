module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Asset ( Asset(assetName, assetType, assetUploadedBy, assetUploadedDate)
                                                     , assetBlob
                                                     , assetRead
                                                     ) where

-- CBG
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Utils

-- other
import           Control.Monad                                     (liftM)
import           Data.DateTime                                     (DateTime, fromSqlString,
                                                                    startOfTime,
                                                                    toSqlString)
import           Data.Maybe                                        (fromMaybe)
import           Data.Text                                         (Text)
import           System.FilePath                                   ((</>))

-- exported
data Asset = Asset { assetRepo         :: Repository
                   , assetPath         :: String
                   , assetName         :: String
                   , assetType         :: String
                   , assetUploadedBy   :: String
                   , assetUploadedDate :: DateTime
                   }
             deriving (Show, Eq)

instance Persistent Asset where
  fromNode node = Asset (node_repo node)
                        (urlToFilePath (node_path node))
                        (node_name node)
                        type'
                        uploadedBy
                        (fromMaybe startOfTime $ fromSqlString uploadedDate)
    where
      StringValue type' = maybe (StringValue "") prop_value $ getProperty node "type"
      StringValue uploadedBy = maybe (StringValue "") prop_value $ getProperty node "uploadedBy"
      StringValue uploadedDate = maybe (StringValue "") prop_value $ getProperty node "uploadedDate"
  toNode _ _ asset = Node (assetName asset)
                          (urlFromString $ assetPath asset)
                          [type', uploadedBy, uploadedDate]
                          (assetRepo asset)
    where
      type' =  Property "type" $ StringValue $ assetType asset
      uploadedBy = Property "uploadedBy" $ StringValue $ assetUploadedBy asset
      uploadedDate = Property "uploadedDate" $ StringValue $ toSqlString $ assetUploadedDate asset

-- exported
assetRead :: Repository -> String -> RepositoryContext Asset
assetRead repo path = liftM fromNode $ getNode repo $ urlFromString path

-- exported
assetBlob :: Asset -> String
assetBlob asset = root (assetRepo asset) </> assetPath asset </> "asset.blob.p"
