{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Foundation where

-- CBG
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.Privileges
import           CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import           CH.ComeBackGloebb.CBGWebSite.Web.Impl.Users
import           CH.ComeBackGloebb.CBGWebSite.Web.Component

-- Yesod
import           Yesod hiding (deleteBy, joinPath, renderBootstrap)
import           Yesod.Auth
import           Yesod.Auth.Account
import           Yesod.Auth.GoogleEmail2
import           Yesod.Static (Static, staticFiles)
import           Network.HTTP.Conduit (Manager)
import           Text.Hamlet (hamletFile)

-- database for auth
import           Database.Persist.Sqlite                            (ConnectionPool,
                                                                     SqlBackend,
                                                                     runSqlPool)

-- standard imports
import           Control.Concurrent (MVar)
import           Control.Monad                                      (liftM)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Network.Mail.Mime (renderSendMail, simpleMail', Address(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
User
    username T.Text
    UniqueUsername username
    password B.ByteString
    emailAddress T.Text
    verified Bool
    verifyKey T.Text
    resetPasswordKey T.Text
    deriving Show
|]

staticFiles "src/main/haskell/CH/ComeBackGloebb/CBGWebSite/Web/static"

data CBGWebSite = CBGWebSite { getStatic    :: Static
                             , getSem       :: MVar Bool
                             , httpManager  :: Manager
                             , components   :: [Component CBGWebSite]
                             , clientId     :: T.Text
                             , clientSecret :: T.Text
                             , dbPool       :: ConnectionPool
                             }

mkYesodData "CBGWebSite" [parseRoutes|
    /                                 RootR                 GET
    /favicon.ico                      FavR                  GET
    /static                           StaticR               Static          getStatic
    /auth                             AuthR                 Auth            getAuth
    /members                          MembersR              GET
    /content/+ContentPath             ContentR              GET
    /edit/content/+ContentPath        EditContentR          GET POST DELETE
    /members/calendar                 MemberCalendarR       GET
    /members/calendar/list            MemberCalendarListR   GET
    /members/calendar/month/#Int/#Int MemberCalendarMR      GET
    /members/event/edit/#T.Text       EventR                GET POST DELETE
    /members/list                     MemberListR           GET
    /members/list/edit/#T.Text        MemberR               GET POST
    /galleries                        GalleriesR            GET
    /gallery/#T.Text                  GalleryR              GET PUT DELETE
    /gallery/#T.Text/images           GalleryImagesR        GET
    /gallery/#T.Text/image/#T.Text    GalleryImageR         GET POST DELETE
    /image/orig/#T.Text/#T.Text       ImageR                GET
    /image/small/#T.Text/#T.Text      ImageSmallR           GET
    /image/thumb/#T.Text/#T.Text      ImageThumbR           GET
    /upload/image/#T.Text             UploadImageR          POST
    /assets/+ContentPath              AssetsR               GET
    /asset/+ContentPath               AssetR                GET POST PUT DELETE
|]

instance Yesod CBGWebSite where
    defaultLayout                             = defaultLayout'
    approot                                   = ApprootStatic "https://comebackgloebb.ch"
    -- isAuthorized route isWriteRequest? = ...
    isAuthorized RootR                  False = return Authorized
    isAuthorized FavR                   False = return Authorized
    isAuthorized (AuthR _)              _     = return Authorized

    -- the members area
    isAuthorized MembersR               False = do
      authUser <- maybeAuthId
      case authUser of
        Just userName | userName `has` Read  `On` Members -> return Authorized
        _                                                 -> return $ Unauthorized ""

    -- the member list
    isAuthorized MemberListR            False = do
      authUser <- maybeAuthId
      case authUser of
        Just userName | userName `has` Read  `On` MemberList -> return Authorized
        _                                                    -> return $ Unauthorized ""

    isAuthorized MemberListR            True  = do
      authUser <- maybeAuthId
      case authUser of
        Just userName | userName `has` Write `On` MemberList -> return Authorized
        _                                                    -> return $ Unauthorized ""

    isAuthorized (MemberR _)            w     = isAuthorized MemberListR w

    -- the event calendar
    isAuthorized  MemberCalendarR       False = isAuthorized MembersR False

    isAuthorized  MemberCalendarListR   False = isAuthorized MembersR False

    isAuthorized (MemberCalendarMR _ _) False = isAuthorized MembersR False

    isAuthorized (EventR _)             w     = isAuthorized MemberListR w

    -- the page content
    isAuthorized (ContentR _)           False = return Authorized

    isAuthorized (EditContentR _)       w     = isAuthorized MemberListR w

    -- the galleries
    isAuthorized GalleriesR             False = isAuthorized MembersR False

    isAuthorized (GalleryR _)           w     = isAuthorized MemberListR w

    isAuthorized (GalleryImagesR _)     False = isAuthorized MembersR False

    isAuthorized (GalleryImageR _ _)    False = isAuthorized MembersR False

    isAuthorized (ImageR _ _)           False = isAuthorized MembersR False

    isAuthorized (ImageSmallR _ _)      False = isAuthorized MembersR False

    isAuthorized (ImageThumbR _ _)      False = isAuthorized MembersR False

    isAuthorized (UploadImageR _)       True  = isAuthorized MembersR False

    -- the assets
    isAuthorized (AssetR _)             w     = isAuthorized MemberListR w
    isAuthorized (AssetsR _)            False = isAuthorized MembersR False

    -- everything else
    isAuthorized _                      _     = return $ Unauthorized ""

instance RenderMessage CBGWebSite FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth CBGWebSite where
    type AuthId CBGWebSite           = T.Text
    getAuthId                        = return . Just . credsIdent
    loginDest _                      = MembersR
    logoutDest _                     = RootR
    authPlugins self                 = [ authGoogleEmail (clientId self) (clientSecret self), accountPlugin ]
    authHttpManager                  = httpManager
    maybeAuthId                      = lookupSession "_ID"

instance YesodPersist CBGWebSite where
    type YesodPersistBackend CBGWebSite = SqlBackend
    runDB action = do
        pool <- liftM dbPool getYesod
        runSqlPool action pool

instance AccountSendEmail CBGWebSite where
    sendVerifyEmail uname email url = do
        $(logInfo) $ T.concat [ "Verification email for "
                              , uname
                              , " (", email, "): "
                              , url
                              ]
        liftIO $ renderSendMail $
          simpleMail' (Address (Just uname) email)
                      (Address (Just "Come Back Glöbb") "info@comebackgloebb.ch")
                      (T.concat ["Verification email for ", uname])
                      (TL.concat ["In order to verify your email address, please click this link: ", TL.fromStrict url])

    sendNewPasswordEmail uname email url = do
        $(logInfo) $ T.concat [ "Reset password email for "
                              , uname
                              , " (", email, "): "
                              , url
                              ]
        liftIO $ renderSendMail $
          simpleMail' (Address (Just uname) email)
                      (Address (Just "Come Back Glöbb") "info@comebackgloebb.ch")
                      (T.concat ["Password reset request for ", uname])
                      (TL.concat ["We have received a password reset request for your account. If you didn't request a password reset, please ignore this email. If the request was indeed yours, please click the link in order to change your password.: ", TL.fromStrict url])


instance YesodAuthAccount (AccountPersistDB CBGWebSite User) CBGWebSite where
    runAccountDB = runAccountPersistDB

instance PersistUserCredentials User where
    userUsernameF = UserUsername
    userPasswordHashF = UserPassword
    userEmailF = UserEmailAddress
    userEmailVerifiedF = UserVerified
    userEmailVerifyKeyF = UserVerifyKey
    userResetPwdKeyF = UserResetPasswordKey
    uniqueUsername = UniqueUsername

    userCreate name email key pwd = User name pwd email False key ""

data ContentPath = ContentPath [T.Text]
    deriving (Read, Show, Eq)

instance PathMultiPiece ContentPath where
    toPathMultiPiece (ContentPath pieces) = pieces
    fromPathMultiPiece = Just . ContentPath . filter (/= "..")

defaultLayout' :: Widget -> Handler Html
defaultLayout' widget = do pageContent     <- widgetToPageContent widget
                           let maybeAuthId' = Nothing :: Maybe (AuthId CBGWebSite)
                           navi            <- widgetToPageContent $ return ()
                           trail           <- widgetToPageContent $ return ()
                           children        <- widgetToPageContent $ return ()
                           withUrlRenderer $(hamletFile "src/main/haskell/CH/ComeBackGloebb/CBGWebSite/Web/Impl/newlayout.hamlet")
