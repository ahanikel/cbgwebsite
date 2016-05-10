module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Gallery ( Gallery(gallery_name, gallery_images)
                                                       , Image(image_name, image_type, image_uploadedBy, image_uploadedDate)
                                                       , list_galleries
                                                       , gallery_create
                                                       , gallery_read
                                                       , gallery_delete
                                                       , gallery_set_image
                                                       , gallery_remove_image
                                                       , image_read
                                                       , image_blob
                                                       ) where

-- CBG
import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository

-- other
import Control.Monad        (filterM)
import Data.ByteString.Lazy (ByteString)
import Data.DateTime        (DateTime, startOfTime, fromSqlString, toSqlString)
import Data.Maybe           (fromMaybe)
import System.FilePath      ((</>))
import Data.Ord             (Ord, compare)
import Data.List            (sort)

data Gallery = Gallery { gallery_repo       :: Repository
                       , gallery_name       :: String
                       , gallery_images     :: [String]
                       , gallery_sort_key   :: String
                       }
               deriving (Show, Eq)

data Image   = Image   { image_repo         :: Repository
                       , image_name         :: String
                       , image_type         :: String
                       , image_gallery      :: String
                       , image_uploadedBy   :: String
                       , image_uploadedDate :: DateTime
                       }
               deriving (Show, Eq)

instance Persistent Gallery where

  fromNode node  = Gallery (node_repo node) (node_name node) [] ""

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

instance Ord Gallery where

  compare a b = compare (key a) (key b)
    where
      key g = gallery_sort_key g ++ gallery_name g

--exported
list_galleries :: Repository -> RepositoryContext [Gallery]
list_galleries repo = do
  root      <- getNode repo $ urlFromString "/"
  gnames    <- getChildNodeNames root
  mapM (gallery_read repo) gnames >>= return . sort

--exported
gallery_create :: Repository -> String -> RepositoryContext Gallery
gallery_create repo name = do
  let gallery = Gallery repo name [] ""
  writeNode $ toNode undefined undefined gallery
  return gallery

--exported
gallery_read   :: Repository -> String -> RepositoryContext Gallery
gallery_read repo gname = do
  gnode                <- getNode repo $ urlFromString gname
  inames               <- getChildNodeNames gnode
  let sortKey           = fromMaybe "" $ fmap show $ fmap prop_value $ getProperty gnode "sortKey"
  return $ Gallery repo gname (sort inames) sortKey

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

--exported
image_read :: Repository -> String -> String -> RepositoryContext Image
image_read repo gname iname = do
    inode    <- getNode repo (urlFromString $ gname </> iname)
    return $ fromNode inode

--exported
image_blob :: Image -> RepositoryContext ByteString
image_blob image = do
    let inode = toNode undefined undefined image
    readBlobProperty inode "image"

