module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Gallery ( Gallery(gallery_name, gallery_images)
                                                       , Image(image_name, image_type, image_uploadedBy, image_uploadedDate)
                                                       , gallery_create
                                                       , gallery_read
                                                       , gallery_delete
                                                       , gallery_set_image
                                                       , gallery_remove_image
                                                       ) where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository

-- other
import Control.Monad        (filterM)
import Data.ByteString.Lazy (ByteString)
import Data.DateTime        (DateTime, startOfTime, fromSqlString, toSqlString)
import Data.Maybe           (fromMaybe)
import System.FilePath      ((</>))


data Gallery = Gallery { gallery_repo       :: Repository
                       , gallery_name       :: String
                       , gallery_images     :: [Image]
                       }
               deriving (Show)

data Image   = Image   { image_repo         :: Repository
                       , image_name         :: String
                       , image_type         :: String
                       , image_gallery      :: String
                       , image_uploadedBy   :: String
                       , image_uploadedDate :: DateTime
                       }
               deriving (Show)

instance Persistent Gallery where

  fromNode node  = Gallery (node_repo node) name []
    where name = node_name node

  toNode _ _ gallery = Node name (urlFromString name) [] (gallery_repo gallery)
    where name = gallery_name gallery

instance Persistent Image where

  fromNode node = Image repo name type' gallery uploadedBy $ fromMaybe startOfTime $ fromSqlString uploadedDate
    where
      repo                     = node_repo node
      name                     = node_name node
      StringValue type'        = fromMaybe (StringValue "") $ fmap prop_value $ getProperty node "type"
      gallery                  = head $ node_path node
      StringValue uploadedBy   = fromMaybe (StringValue "") $ fmap prop_value $ getProperty node "uploadedBy"
      StringValue uploadedDate = fromMaybe (StringValue "") $ fmap prop_value $ getProperty node "uploadedDate"

  toNode _ _ image = Node name url [type', uploadedBy, uploadedDate] repo
    where
      name         = image_name    image
      gallery      = image_gallery image
      url          = urlFromString $ gallery </> name
      type'         = Property "type"         $ StringValue $ image_type image
      uploadedBy   = Property "uploadedBy"   $ StringValue $ image_uploadedBy image
      uploadedDate = Property "uploadedDate" $ StringValue $ toSqlString $ image_uploadedDate image
      repo         = image_repo    image

--exported
gallery_create :: Repository -> String -> RepositoryContext Gallery
gallery_create repo name = do
  let gallery = Gallery repo name []
  writeNode $ toNode undefined undefined gallery
  return gallery

--exported
gallery_read   :: Repository -> String -> RepositoryContext Gallery
gallery_read repo name = do
  node           <- getNode repo $ urlFromString name
  childnodes     <- getChildNodeNames node
  let gallery     = fromNode node
  let image_url   = urlFromString . (gallery_name gallery </>)
  image_nodes    <- mapM (getNode repo . image_url) childnodes
  let images      = map fromNode image_nodes
  return gallery { gallery_images = images }

--exported
gallery_delete :: Repository -> String -> RepositoryContext ()
gallery_delete repo name = getNode repo (urlFromString name) >>= deleteNode

--exported
gallery_set_image :: Gallery -> String -> String -> ByteString -> String -> DateTime -> RepositoryContext Gallery
gallery_set_image gallery name type' bytes uploadedBy uploadedDate = do
  let image = Image (gallery_repo gallery)
                    name
                    type'
                    (gallery_name gallery)
                    uploadedBy
                    uploadedDate
      node  = toNode undefined undefined image
  writeNode node
  writeBlobProperty node "image" bytes
  gallery_read (gallery_repo gallery) (gallery_name gallery)

--exported
gallery_remove_image :: Gallery -> String -> RepositoryContext Gallery
gallery_remove_image gallery name = do
  let gnode = toNode undefined undefined gallery
  inode    <- getChildNode gnode name
  deleteNode inode
  gallery_read (gallery_repo gallery) (gallery_name gallery)
