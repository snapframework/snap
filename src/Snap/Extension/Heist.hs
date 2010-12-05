{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

'Snap.Extension.Heist' exports the 'MonadHeist' interface which allows you to
integrate Heist templates into your Snap application. The interface's
operations are 'heistServe', 'heistServeSingle', 'heistLocal' and 'render'.

'Snap.Extension.Heist.Heist' contains the only implementation of this
interface and can be used to turn your application's monad into a
'MonadHeist'.

'MonadHeist' is unusual among Snap extensions in that it's a multi-parameter
typeclass. The last parameter is your application's monad, and the first is
the monad you want the 'TemplateState' to use. This is usually, but not
always, also your application's monad.

-}

module Snap.Extension.Heist (MonadHeist(..)) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Snap.Types
import           Text.Templating.Heist


------------------------------------------------------------------------------
-- | The 'MonadHeist' type class. Minimal complete definition: 'render',
-- 'heistLocal'.
class (Monad n, MonadSnap m) => MonadHeist n m | m -> n where
    -- | Renders a template as text\/html. If the given template is not found,
    -- this returns 'empty'.
    render     :: ByteString -> m ()

    -- | Runs an action with a modified 'TemplateState'. You might want to use
    -- this if you had a set of splices which were customised for a specific
    -- action. To do that you would do:
    --
    -- > heistLocal (bindSplices mySplices) $ render "myTemplate"
    heistLocal :: (TemplateState n -> TemplateState n) -> m a -> m a

    -- | Analogous to 'fileServe'. If the template specified in the request
    -- path is not found, it returns 'empty'.
    heistServe :: m ()
    heistServe = fmap rqPathInfo getRequest >>= render

    -- | Analogous to 'fileServeSingle'. If the given template is not found,
    -- this throws an error.
    heistServeSingle :: ByteString -> m ()
    heistServeSingle t = render t
        <|> error ("Template " ++ show t ++ " not found.")
