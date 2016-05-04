module CH.ComeBackGloebb.CBGWebSite.Web.Impl.Privileges ( Privilege(..)
                                                        , Section(..)
                                                        , Permission(..)
                                                        ) where

data Privilege = Create
               | Read
               | Write
               | Modify
               | Delete

data Section = Members
             | MemberList
             | MemberCalendar

data Permission = On Privilege Section
