module Control.Access.RoleBased.Internal.Rule where

------------------------------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List (foldl')
import           Data.Monoid
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Control.Access.RoleBased.Internal.Role


------------------------------------------------------------------------------
data Rule = Rule Text (Role -> [Role])


------------------------------------------------------------------------------
newtype RuleSet = RuleSet (HashMap Text (Role -> [Role]))


------------------------------------------------------------------------------
instance Monoid RuleSet where
    mempty = RuleSet M.empty
    (RuleSet m1) `mappend` (RuleSet m2) = RuleSet $ M.foldlWithKey' ins m2 m1
      where
        combine f1 f2 r = f1 r ++ f2 r
        ins m k v       = M.insertWith combine k v m


------------------------------------------------------------------------------
ruleToSet :: Rule -> RuleSet
ruleToSet (Rule nm f) = RuleSet $ M.singleton nm f


------------------------------------------------------------------------------
rulesToSet :: [Rule] -> RuleSet
rulesToSet = foldl' mappend (RuleSet M.empty) . map ruleToSet
