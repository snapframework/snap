module Control.Access.RoleBased.Internal.RoleMap where

------------------------------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import           Data.List (find, foldl')
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Control.Access.RoleBased.Role
import           Control.Access.RoleBased.Internal.Types


------------------------------------------------------------------------------
newtype RoleMap = RoleMap (HashMap Text (HashSet Role))


------------------------------------------------------------------------------
fromList :: [Role] -> RoleMap
fromList = RoleMap . foldl' ins M.empty
  where
    ins m role =
        M.insertWith S.union (_roleName role) (S.singleton role) m


------------------------------------------------------------------------------
lookup :: Role -> RoleMap -> Maybe Role
lookup role (RoleMap m) = find (`matches` role) l
  where
    l = maybe [] S.toList $ M.lookup (_roleName role) m


------------------------------------------------------------------------------
delete :: Role -> RoleMap -> RoleMap
delete role (RoleMap m) = RoleMap $ maybe m upd $ M.lookup rNm m
  where
    rNm = _roleName role
    upd s = maybe m
                  (\r -> let s' = S.delete r s
                         in if S.null s'
                              then M.delete rNm m
                              else M.insert rNm s' m)
                  (find (`matches` role) $ S.toList s)


------------------------------------------------------------------------------
insert :: Role -> RoleMap -> RoleMap
insert role (RoleMap m) =
    RoleMap $ M.insertWith S.union (_roleName role) (S.singleton role) m


------------------------------------------------------------------------------
empty :: RoleMap
empty = RoleMap M.empty


------------------------------------------------------------------------------
null :: RoleMap -> Bool
null (RoleMap m) = M.null m
