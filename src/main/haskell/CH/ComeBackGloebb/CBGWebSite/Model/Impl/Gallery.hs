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
                                                       , image_small
                                                       , image_thumb
                                                       , image_write
                                                       ) where

-- CBG
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Utils

-- other
import           Control.Monad                                     (unless,
                                                                    when)
import           Data.ByteString.Lazy                              (ByteString)
import           Data.DateTime                                     (DateTime, fromSqlString,
                                                                    getCurrentTime,
                                                                    startOfTime,
                                                                    toSqlString)
import           Data.List                                         (sort)
import           Data.Maybe                                        (fromMaybe,
                                                                    isJust)
import           System.FilePath                                   ((</>))
import           System.Process                                    (callProcess)

data Gallery = Gallery { gallery_repo     :: Repository
                       , gallery_name     :: String
                       , gallery_images   :: [String]
                       , gallery_sort_key :: String
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
      type'        = Property "type"         $ StringValue $ image_type image
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
  root'     <- getNode repo $ urlFromString "/"
  gnames    <- getChildNodeNames root'
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

image_blob_name :: String
image_blob_name = "image.blob"

image_small_name :: String
image_small_name = "image.small"

image_thumb_name :: String
image_thumb_name = "image.thumb"

--exported
image_read :: Repository -> String -> String -> RepositoryContext Image
image_read repo gname iname = do
    inode         <- getNode repo (urlFromString $ gname </> iname)
    let image      = fromNode inode
    let blobPath   = image_blob  image
    let smallPath  = image_small image
    let thumbPath  = image_thumb image
    let mBlobProp  = getProperty inode image_blob_name
    let mSmallProp = getProperty inode image_small_name
    let mThumbProp = getProperty inode image_thumb_name
    let hasBlob    = isJust mBlobProp
    let hasSmall   = isJust mSmallProp
    let hasThumb   = isJust mThumbProp
    unless hasThumb $ when hasBlob $ check $ callProcess "/usr/local/bin/convert" [blobPath, "-resize", "256", thumbPath]
    unless hasSmall $ when hasBlob $ check $ callProcess "/usr/local/bin/convert" [blobPath, "-resize", "1010", smallPath]
    return image

--exported: shamelessly changing type signature, we haven't released yet
image_blob :: Image -> FilePath
image_blob image =
  let inode = toNode undefined undefined image
  in getPropertyPath inode image_blob_name

image_thumb :: Image -> FilePath
image_thumb image =
  let inode = toNode undefined undefined image
  in getPropertyPath inode image_thumb_name

image_small :: Image -> FilePath
image_small image =
  let inode = toNode undefined undefined image
  in getPropertyPath inode image_small_name

--exported
image_write :: Repository -> String -> String -> String -> String -> ByteString -> RepositoryContext ()
image_write repo gname iname contentType uploadedBy bytes = do
    now <- check getCurrentTime
    let image = Image repo iname contentType gname uploadedBy now
    let inode = toNode undefined undefined image
    writeNode inode
    writeBlobProperty inode "image" bytes
