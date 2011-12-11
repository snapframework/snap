{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Access.RoleBased.Checker where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Logic
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Control.Access.RoleBased.Internal.RoleMap (RoleMap)
import qualified Control.Access.RoleBased.Internal.RoleMap as RM
import           Control.Access.RoleBased.Internal.Types
import           Control.Access.RoleBased.Role


------------------------------------------------------------------------------
type RoleBuilder a = StateT RoleMap RoleMonad a


------------------------------------------------------------------------------
applyRule :: Role -> Rule -> [Role]
applyRule r (Rule _ f) = f r


------------------------------------------------------------------------------
applyRuleSet :: Role -> RuleSet -> [Role]
applyRuleSet r (RuleSet m) = f r
  where
    f = fromMaybe (const []) $ M.lookup (_roleName r) m


------------------------------------------------------------------------------
checkUnseen :: Role -> RoleBuilder ()
checkUnseen role = do
    m <- get
    if isJust $ RM.lookup role m then mzero else return ()


------------------------------------------------------------------------------
checkSeen :: Role -> RoleBuilder ()
checkSeen = lnot . checkUnseen


------------------------------------------------------------------------------
markSeen :: Role -> RoleBuilder ()
markSeen role = modify $ RM.insert role


------------------------------------------------------------------------------
isum :: (MonadLogic m, MonadPlus m) => [m a] -> m a
isum l = case l of
            []     -> mzero
            (x:xs) -> x `interleave` isum xs


------------------------------------------------------------------------------
-- | Given a set of roles to check, and a set of implication rules describing
-- how a given role inherits from other roles, this function produces a stream
-- of expanded Roles. If a Role is seen twice, expandRoles mzeros.
expandRoles :: [Rule] -> [Role] -> RoleMonad Role
expandRoles rules roles0 = evalStateT (go roles0) RM.empty
  where
    ruleSet = rulesToSet rules

    go roles = isum $ map expandOne roles

    expandOne role = do
        checkUnseen role
        markSeen role
        return role `interleave` go newRoles

      where
        newRoles = applyRuleSet role ruleSet


------------------------------------------------------------------------------
hasRole :: Role -> RuleChecker ()
hasRole r = RuleChecker $ do
    ch <- ask
    once $ go ch
  where
    go gen = do
        r' <- lift gen
        if r `matches` r' then return () else mzero


------------------------------------------------------------------------------
missingRole :: Role -> RuleChecker ()
missingRole = lnot . hasRole


------------------------------------------------------------------------------
hasAllRoles :: [Role] -> RuleChecker ()
hasAllRoles rs = RuleChecker $ do
    ch <- ask
    lift $ once $ go ch $ RM.fromList rs
  where
    go gen !st = do
        mr <- msplit gen
        maybe mzero
              (\(r,gen') -> let st' = RM.delete r st
                            in if RM.null st'
                                 then return ()
                                 else go gen' st')
              mr


------------------------------------------------------------------------------
hasAnyRoles :: [Role] -> RuleChecker ()
hasAnyRoles rs = RuleChecker $ do
    ch <- ask
    lift $ once $ go ch
  where
    st = RM.fromList rs
    go gen = do
        mr <- msplit gen
        maybe mzero
              (\(r,gen') -> if isJust $ RM.lookup r st
                                 then return ()
                                 else go gen')
              mr


------------------------------------------------------------------------------
runRuleChecker :: [Rule]
               -> [Role]
               -> RuleChecker a
               -> Bool
runRuleChecker rules roles (RuleChecker f) =
    case outs of
      []    -> False
      _     -> True
  where
    (RoleMonad st) = runReaderT f $ expandRoles rules roles
    outs = observeMany 1 st


------------------------------------------------------------------------------
mkRule :: Text -> (Role -> [Role]) -> Rule
mkRule = Rule


------------------------------------------------------------------------------
implies :: Role -> [Role] -> Rule
implies src dest = Rule (_roleName src)
                        (\role -> if role `matches` src then dest else [])


------------------------------------------------------------------------------
impliesWith :: Role -> (HashMap Text RoleValue -> [Role]) -> Rule
impliesWith src f = Rule (_roleName src)
                         (\role -> if src `matches` role
                                     then f $ _roleData role
                                     else [])


------------------------------------------------------------------------------
-- Testing code follows: TODO: move into test suite


testRules :: [Rule]
testRules = [ "user" `implies` ["guest", "can_post"]
            , "superuser" `implies` [ "user"
                                    , "can_moderate"
                                    , "can_administrate"]
            , "superuser" `implies` [ addRoleData "arg" "*" "with_arg" ]
            , "with_arg" `impliesWith` \dat ->
                maybe [] (\arg -> [addRoleData "arg" arg "dependent_arg"]) $
                      M.lookup "arg" dat
            , "superuser" `implies` [ addRoleData "arg1" "a" $
                                      addRoleData "arg2" "b" "multi_args" ]
            ]

tX :: RuleChecker () -> Bool
tX f = runRuleChecker testRules ["superuser"] f

t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17 :: Bool
t1 = tX $ hasAnyRoles ["guest","userz"]

t2 = tX $ hasAllRoles ["guest","userz"]

t3 = tX $ hasAllRoles ["guest","user"]

t4 = tX $ hasRole "can_administrate"

t5 = tX $ hasRole "lkfdhjkjfhds"

t6 = tX $ do
         hasRole "guest"
         hasRole "superuser"

t7 = tX $ do
         hasRole "zzzzz"
         hasRole "superuser"

t8 = tX $ hasRole $ addRoleData "arg" "*" "dependent_arg"

t9 = tX $ hasRole "multi_args"

t10 = tX $ hasRole $ addRoleData "arg2" "b" "multi_args"

t11 = tX $ hasRole $ addRoleData "arg2" "z" "multi_args"

t12 = tX $ hasAllRoles [addRoleData "arg2" "b" "multi_args"]

t13 = tX $ hasAnyRoles [ addRoleData "arg2" "z" "multi_args"
                       , addRoleData "arg2" "b" "multi_args" ]

t14 = tX $ hasAnyRoles [ addRoleData "arg2" "z" "multi_args"
                       , addRoleData "arg2" "aaa" "multi_args" ]

t15 = tX $ missingRole "jflsdkjf"

t16 = tX $ do
          missingRole "fdjlksjlf"
          hasRole "multi_args"

t17 = tX $ missingRole "multi_args"
