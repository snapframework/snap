-- | An internal module that copies a few select functions
-- from Control.Error.Util, as used in Snap.Snaplet.Auth.Handlers.
module Snap.Snaplet.Auth.Handlers.Errors
  ( hush
  , hushT
  , note
  , noteT
  , hoistMaybe
  ) where

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe

-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-- | Suppress the 'Left' value of an 'EitherT'
hushT :: (Monad m) => EitherT a m b -> MaybeT m b
hushT = MaybeT . liftM hush . runEitherT

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

-- | Tag the 'Nothing' value of a 'MaybeT'
noteT :: (Monad m) => a -> MaybeT m b -> EitherT a m b
noteT a = EitherT . liftM (note a) . runMaybeT

-- | Lift a 'Maybe' to the 'MaybeT' monad
hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . return
