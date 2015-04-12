{-# LANGUAGE OverloadedStrings #-}

module Users (has) where

import Privileges
import Data.Text (Text)

infixl 5 `has`
has :: Text -> Permission -> Bool

"me@home.com"               `has` Read  `On` Members        = True

_                           `has` _     `On` _              = False
