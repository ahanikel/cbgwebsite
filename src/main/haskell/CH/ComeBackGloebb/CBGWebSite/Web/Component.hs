{-# LANGUAGE OverloadedStrings #-}

module CH.ComeBackGloebb.CBGWebSite.Web.Component where

import CH.ComeBackGloebb.CBGWebSite.Repo.Impl.Repository
import CH.ComeBackGloebb.CBGWebSite.Model.Impl.Navigation

import Yesod

import qualified Data.Text as T
import           Data.String (IsString(..))

data Component a = Component
                   { compName          :: T.Text
                   , compNaviName      :: T.Text
                   , compNaviTrail     :: [T.Text] -> WidgetT a IO ()
                   , compNaviChildren  :: [T.Text] -> WidgetT a IO ()
                   , compRepository    :: Repository
                   }
                 | ComponentName
                   { compName          :: T.Text
                   }

instance Eq (Component a) where
  c1 == c2 = compName c1 == compName c2

instance IsString (Component a) where
  fromString s = ComponentName (T.pack s)
