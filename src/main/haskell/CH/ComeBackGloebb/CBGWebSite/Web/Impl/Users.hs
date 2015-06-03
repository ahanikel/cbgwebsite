{-# LANGUAGE OverloadedStrings #-}

module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Users (has) where

import CH.ComeBackGloebb.CBGWebSite.Web.Impl.Privileges
import Data.Text (Text)

infixl 5 `has`
has :: Text -> Permission -> Bool

"me@home.com"               `has` Read  `On` Members        = True

_                           `has` _     `On` _              = False
