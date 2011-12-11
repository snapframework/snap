module Control.Access.RoleBased.Role where

------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as M
import           Control.Access.RoleBased.Internal.Types
import           Data.Text (Text)


------------------------------------------------------------------------------
matches :: Role -> Role -> Bool
matches (Role a1 d1) (Role a2 d2) =
    a1 == a2 && dmatch (toSortedList d1) (toSortedList d2)
  where
    dmatch []         _      = True
    dmatch _          []     = False
    dmatch dds@(d:ds) (e:es) =
        case compare d e of
          LT -> False
          EQ -> dmatch ds es
          GT -> dmatch dds es


------------------------------------------------------------------------------
addRoleData :: Text -> RoleValue -> Role -> Role
addRoleData k v (Role n d) = Role n $ M.insert k v d
