module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Privileges ( Privilege(..)
                                                        , Section(..)
                                                        , Permission(..)
                                                        ) where

data Privilege = Create
               | Read
               | Modify
               | Delete

data Section = Members

data Permission = On Privilege Section
