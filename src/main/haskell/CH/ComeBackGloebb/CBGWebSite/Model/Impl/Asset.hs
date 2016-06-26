module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Asset ( Asset(assetName, assetType, assetUploadedBy, assetUploadedDate)
                                                     , assetBlob
                                                     , assetRead
                                                     ) where

-- CBG
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository

-- other
import           Control.Monad                                     (liftM)
import qualified Data.ByteString.Lazy.UTF8                         as UL8
import           Data.DateTime                                     (DateTime, fromSqlString,
                                                                    startOfTime,
                                                                    toSqlString)
import           Data.Maybe                                        (fromMaybe)
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

  writeItem _ = undefined

  deleteItem _ = undefined

  readItem repo path = do
    node         <- getNode repo $ urlFromString path
    type'        <- liftM UL8.toString $ getProperty node "type"
    uploadedBy   <- liftM UL8.toString $ getProperty node "uploadedBy"
    uploadedDate <- liftM UL8.toString $ getProperty node "uploadedDate"
    return $ Asset (node_repo node)
                   (urlToFilePath (node_path node))
                   (node_name node)
                   type'
                   uploadedBy
                   (fromMaybe startOfTime $ fromSqlString uploadedDate)

  toNode asset = Node (assetName asset)
                      (urlFromString $ assetPath asset)
                      []
                      (assetRepo asset)

-- exported
assetRead :: Repository -> String -> RepositoryContext Asset
assetRead = readItem

-- exported
assetBlob :: Asset -> String
assetBlob asset = root (assetRepo asset) </> assetPath asset </> "asset.blob.p"
