{-# LANGUAGE OverloadedStrings #-}

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
import           Control.Monad                                     (liftM,
                                                                    unless,
                                                                    when)
import qualified Data.ByteString.Lazy                              as BL
import qualified Data.ByteString.Lazy.UTF8                         as UL8
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

data Image   = Image   { image_repo         :: Repository
                       , image_name         :: String
                       , image_type         :: String
                       , image_gallery      :: String
                       , image_uploadedBy   :: String
                       , image_uploadedDate :: DateTime
                       }

instance Eq Gallery where

  g1 == g2 =
    gallery_name g1 == gallery_name g2 &&
    gallery_repo g1 == gallery_repo g2

instance Ord Gallery where

  compare a b = compare (key a) (key b)
    where
      key g = gallery_sort_key g ++ gallery_name g

instance Persistent Gallery where

  -- TODO: write the properties
  writeItem g = writeNode $ toNode g

  readItem repo gname = do
    gnode   <- getNode repo $ urlFromString gname
    inames  <- getChildNodeNames gnode
    sortKey <- liftM UL8.toString $ getPropertyWithDefault gnode "sortKey" ""
    return $ Gallery repo gname (sort inames) sortKey

  deleteItem g = deleteNode $ toNode g

  toNode g = Node name (urlFromString name) [] repo
    where name = gallery_name g
          repo = gallery_repo g

instance Persistent Image where

  -- TODO: write the properties
  writeItem i = writeNode $ toNode i

  readItem repo path = do
    inode        <- getNode repo (urlFromString path)
    let name      = node_name inode
    type'        <- liftM UL8.toString $ getProperty inode "type"
    let gallery   = head $ node_path inode
    uploadedBy   <- liftM UL8.toString $ getProperty inode "uploadedBy"
    uploadedDate <- liftM (fromMaybe startOfTime . fromSqlString . UL8.toString) $ getProperty inode "uploadedDate"
    return $ Image repo name type' gallery uploadedBy uploadedDate

  deleteItem i = do
    let repo    = image_repo i
    let gname   = image_gallery i
    let iname   = image_name i
    inode      <- getNode repo (urlFromString $ gname </> iname)
    deleteNode inode

  toNode i = Node imageName (urlFromString imagePath) [] repo
    where
      gallery     = image_gallery i
      imageName   = image_name i
      imagePath   = gallery </> imageName
      repo        = image_repo i

--exported
list_galleries :: Repository -> RepositoryContext [Gallery]
list_galleries repo = do
  root'     <- getNode repo $ urlFromString "/"
  gnames    <- getChildNodeNames root'
  mapM (gallery_read repo) gnames >>= return . sort

--exported
gallery_create :: Repository -> String -> RepositoryContext Gallery
gallery_create repo name = do
  let g = Gallery repo name [] ""
  writeItem g
  return g

--exported
gallery_read :: Repository -> String -> RepositoryContext Gallery
gallery_read = readItem

--exported
gallery_delete :: Repository -> String -> RepositoryContext ()
gallery_delete repo name = (readItem repo name :: RepositoryContext Gallery) >>= deleteItem

--exported
gallery_set_image :: Gallery -> String -> String -> BL.ByteString -> String -> DateTime -> RepositoryContext Gallery
gallery_set_image gallery name type' bytes uploadedBy uploadedDate = do
  let image = Image (gallery_repo gallery)
                    name
                    type'
                    (gallery_name gallery)
                    uploadedBy
                    uploadedDate
      node  = toNode image
  writeNode node
  writeProperty node image_blob_name bytes
  gallery_read (gallery_repo gallery) (gallery_name gallery)

--exported
gallery_remove_image :: Gallery -> String -> RepositoryContext Gallery
gallery_remove_image gallery name = do
  let gnode = toNode gallery
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
    image         <- readItem repo (gname </> iname)
    let inode      = toNode image
        blobPath   = image_blob  image
        smallPath  = image_small image
        thumbPath  = image_thumb image
    hasBlob       <- hasProperty inode image_blob_name
    hasSmall      <- hasProperty inode image_small_name
    hasThumb      <- hasProperty inode image_thumb_name
    unless hasThumb $ when hasBlob $ check $ callProcess "/usr/local/bin/convert" [blobPath, "-resize", "256", thumbPath]
    unless hasSmall $ when hasBlob $ check $ callProcess "/usr/local/bin/convert" [blobPath, "-resize", "1010", smallPath]
    return image

getImagePropertyPath :: Image -> String -> FilePath
getImagePropertyPath i p =
  let inode     = toNode i
  in getPropertyPath inode p

--exported: shamelessly changing type signature, we haven't released yet
image_blob :: Image -> FilePath
image_blob image = getImagePropertyPath image image_blob_name

image_thumb :: Image -> FilePath
image_thumb image = getImagePropertyPath image image_thumb_name

image_small :: Image -> FilePath
image_small image = getImagePropertyPath image image_small_name

--exported
image_write :: Repository -> String -> String -> String -> String -> BL.ByteString -> RepositoryContext ()
image_write repo gname iname contentType uploadedBy bytes = do
    now <- check getCurrentTime
    writeItem $ Image repo iname contentType gname uploadedBy now
    inode <- getNode repo (urlFromString $ gname </> iname)
    writeProperty inode image_blob_name bytes
