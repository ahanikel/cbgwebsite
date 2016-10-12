{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Asset ( Asset(assetRepo, assetName, assetPath, assetType, assetUploadedBy, assetUploadedDate)
                                                     , assetBlob
                                                     , assetRead
                                                     , assetWrite
                                                     , listAssets
                                                     ) where

-- CBG
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository

-- other
import           Control.Monad                                     (liftM)
import qualified Data.ByteString.Lazy                              as BL
import qualified Data.ByteString.Lazy.UTF8                         as UL8
import           Data.DateTime                                     (DateTime, fromSqlString,
                                                                    startOfTime,
                                                                    toSqlString)
import           Data.Maybe                                        (fromMaybe)
import           Debug.Trace
import           System.FilePath                                   ((</>))

-- exported
data Asset = Asset { assetRepo         :: Repository
                   , assetPath         :: URL
                   , assetName         :: String
                   , assetType         :: String
                   , assetUploadedBy   :: String
                   , assetUploadedDate :: DateTime
                   }
             deriving (Show, Eq, Read)

instance Persistent Asset where

  writeItem a @ Asset {..} = do
    let n = toNode a
    writeNode n
    writeProperty n "type"         $ UL8.fromString assetType
    writeProperty n "uploadedBy"   $ UL8.fromString assetUploadedBy
    writeProperty n "uploadedDate" $ UL8.fromString $ toSqlString assetUploadedDate

  deleteItem = deleteNode . toNode

  readItem repo path = do
    node         <- getNode repo $ urlFromString path
    type'        <- liftM UL8.toString $ getPropertyWithDefault node "type" "application/x-directory"
    uploadedBy   <- liftM UL8.toString $ getPropertyWithDefault node "uploadedBy" "nobody"
    uploadedDate <- liftM UL8.toString $ getPropertyWithDefault node "uploadedDate" "1970-01-01T00:00:00"
    return $ Asset (node_repo node)
                   (node_path node)
                   (node_name node)
                   type'
                   uploadedBy
                   (fromMaybe startOfTime $ fromSqlString uploadedDate)

  toNode asset = Node (assetName asset)
                      (assetPath asset)
                      []
                      (assetRepo asset)

-- exported
assetRead :: Repository -> String -> RepositoryContext Asset
assetRead = readItem

-- exported
-- we're crossing abstraction boundaries here but we have to if we want to use sendFile
assetBlob :: Asset -> String
assetBlob asset = root (assetRepo asset) </> urlToFilePath (assetPath asset) </> "asset.blob.p"

-- exported
assetWrite :: Repository -> [String] -> String -> String -> String -> DateTime -> BL.ByteString -> RepositoryContext ()
assetWrite repo path name type' uploadedBy uploadedDate blob = do
  let asset = Asset repo path name type' uploadedBy uploadedDate
      node  = toNode asset
  writeItem asset
  writeProperty node "asset.blob" blob

-- exported
listAssets :: Repository -> [String] -> RepositoryContext [Asset]
listAssets repo path = do
  node <- getNode repo path
  children <- getChildNodeNames node
  let children' = map (\c -> node_path node ++ [c]) children
  mapM (assetRead repo . urlToString) children'
