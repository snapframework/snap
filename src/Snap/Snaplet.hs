{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeOperators              #-}

{-|

Snaplets allow you to build web applications out of composable parts. This
allows you to build self-contained units and glue them together to make your
overall application.

A snaplet has a few moving parts, some user-defined and some provided by the
snaplet API:

  * each snaplet has its own configuration given to it at startup.

  * each snaplet is given its own directory on the filesystem, from which it
    reads its configuration and in which it can store files.

  * each snaplet comes with an 'Initializer' which defines how to create an
    instance of the Snaplet at startup. The initializer decides how to
    interpret the snaplet configuration, which URLs to handle (and how), sets
    up the initial snaplet state, tells the snaplet runtime system how to
    clean the snaplet up, etc.

  * each snaplet contains some user-defined in-memory state; for instance, a
    snaplet that talks to a database might contain a reference to a connection
    pool. The snaplet state is an ordinary Haskell record, with a datatype
    defined by the snaplet author. The initial state record is created during
    initialization and is available to snaplet 'Handler's when serving HTTP
    requests.

NOTE: This documentation is written as a prose tutorial of the snaplets
API.  Don't be scared by the fact that it's auto-generated and is filled with
type signatures.  Just keep reading.

-}

module Snap.Snaplet
  (
  -- * Snaplet
  -- $snapletDoc
    Snaplet
  , SnapletConfig

  -- * Lenses
  -- $lenses

  -- * Snaplet Helper Functions
  -- $snapletHelpers
  , snapletConfig
  , snapletValue
  , subSnaplet

  -- * MonadSnaplet
  -- $monadSnaplet
  , MonadSnaplet(..)
  , getSnapletAncestry
  , getSnapletFilePath
  , getSnapletName
  , getSnapletDescription
  , getSnapletUserConfig
  , getSnapletRootURL
  , snapletURL
  , getRoutePattern
  , setRoutePattern

  -- * Snaplet state manipulation
  -- $snapletState
  , getSnapletState
  , putSnapletState
  , modifySnapletState
  , getsSnapletState

--  , wrap
--  , wrapTop

  -- * Initializer
  -- $initializer
  , Initializer
  , SnapletInit
  , makeSnaplet

  , nestSnaplet
  , embedSnaplet
  , nameSnaplet

  , onUnload
  , addPostInitHook
  , addPostInitHookBase
  , printInfo
  , getRoutes

  -- * Routes
  -- $routes
  , addRoutes
  , wrapSite

  -- * Handlers
  , Handler
  , failIfNotLocal
  , reloadSite
  , modifyMaster
  , bracketHandler

  -- * Serving Applications
  , runSnaplet
  , combineConfig
  , serveSnaplet
  , loadAppConfig

  -- * Snaplet Lenses
  , SnapletLens
  ) where


import           Snap.Snaplet.Internal.Initializer
import           Snap.Snaplet.Internal.Types

-- $snapletDoc
-- The heart of the snaplets infrastructure is state management. (Note: when
-- we say \"state\" here, we mean in-memory Haskell objects, not external data
-- storage or databases; how you deal with persisted data is up to you.) Most
-- nontrivial pieces of a web application need some kind of runtime state or
-- environment data. The datatype we use to handle this is called 'Snaplet':

-- $snapletHelpers
--
-- Your web application will itself get wrapped in a 'Snaplet', and the
-- top-level user state of your application (which will likely contain other
-- snaplets nested inside it) will look something like this:
--
-- > data App = App
-- >     { _foo                :: Snaplet Foo
-- >     , _bar                :: Snaplet Bar
-- >     , _someNonSnapletData :: String
-- >     }
--
-- Every web application using snaplets has a top-most user state which
-- contains all of the application state; we call this state the \"base\"
-- state.
--
-- We export several helper lenses for working with Snaplet types.

-- $lenses
-- In the example above, the @Foo@ snaplet has to be written to work with any
-- base state (otherwise it wouldn't be reusable!), but functions written to
-- work with the @Foo@ snaplet want to be able to modify the @Foo@ record
-- /within the context/ of the base state. Given that Haskell datatypes are
-- pure, how do you allow for this?
--
-- Our solution is to use /lenses/, as defined in Edward Kmett's @lens@
-- library (<http://hackage.haskell.org/package/lens>). A lens, notated
-- as follows:
--
-- > SimpleLens a b
--
-- is conceptually a \"getter\" and a \"setter\" rolled up into one. The
-- @lens@ library provides the following functions:
--
-- > view :: (SimpleLens a b) -> a -> b
-- > set  :: (SimpleLens a b) -> b -> a -> a
-- > over :: (SimpleLens a b) -> (b -> b) -> a -> a
--
-- which allow you to get, set, and modify a value of type @b@ within the
-- context of type @a@. The @lens@ package comes with a Template Haskell
-- function called 'makeLenses', which auto-magically defines a lens for every
-- record field having a name beginning with an underscore. In the @App@
-- example above, adding the declaration:
--
-- > makeLenses ''App
--
-- would define lenses:
--
-- > foo                :: SimpleLens App (Snaplet Foo)
-- > bar                :: SimpleLens App (Snaplet Bar)
-- > someNonSnapletData :: SimpleLens App String
--
-- The coolest thing about @lens@ lenses is that they /compose/ using the
-- @(.)@ operator. If the @Foo@ type had a field of type @Quux@ within it with
-- a lens @quux :: SimpleLens Foo Quux@, then you could create a lens of type
-- @SimpleLens App Quux@ by composition:
--
-- > import Control.Lens
-- >
-- > data Foo = Foo { _quux :: Quux }
-- > makeLenses ''Foo
-- >
-- > -- snapletValue is defined in the framework:
-- > snapletValue :: SimpleLens (Snaplet a) a
-- >
-- > appQuuxLens :: SimpleLens App Quux
-- > appQuuxLens = foo . snapletValue . quux
--
-- Lens composition is very similar to function composition except it works in
-- the opposite direction (think Java-style System.out.println ordering) and
-- it gives you a composed getter and setter at the same time.

-- $monadSnaplet
-- The primary abstraction in the snaplet infrastructure is a combination of
-- the reader and state monads.  The state monad holds the top level
-- application data type (from now on referred to as the base state).  The
-- reader monad holds a lens from the base state to the current snaplet's
-- state.  This allows quux snaplet functions to access and modify the Quux
-- data structure without knowing anything about the App or Foo data
-- structures. It also lets other snaplets call functions from the quux
-- snaplet if they have the quux snaplet's lens @SimpleLens App (Snaplet Quux)@.
-- We can view our application as a tree of snaplets and other pieces of data.
-- The lenses are like pointers to nodes of the tree. If you have a pointer to
-- a node, you can access the node and all of its children without knowing
-- anything about the rest of the tree.
--
-- Several monads use this infrastructure. These monads need at least three
-- type parameters. Two for the lens type, and the standard \'a\' denoting the
-- monad return value. You will usually see this written in type signatures as
-- \"m b v a\" or some variation. The \'m\' is the type variable of the
-- MonadSnaplet type class. \'b\' is the base state, and \'v\' is the state of
-- the current \"view\" snaplet (or simply, current state).
--
-- The MonadSnaplet type class distills the essence of the operations used
-- with this pattern.  Its functions define fundamental methods for navigating
-- snaplet trees.

-- $snapletState
-- MonadSnaplet instances will typically have @MonadState v@ instances.  We
-- provide the following convenience functions which give the equivalent to
-- @MonadState (Snaplet v)@ for the less common cases where you need to work
-- with the Snaplet wrapper.

-- $initializer
-- The Initializer monad is where your application's initialization happens.
-- Initializers are run at startup and any time a site reload is triggered.
-- The Initializer's job is to construct a snaplet's routes and initial state,
-- set up filesystem data, read config files, etc.
--
-- In order to initialize its state, a snaplet needs to initialize all the
-- @Snaplet a@ state for each of its subsnaplets.  The only way to construct
-- a @Snaplet a@ type is by calling 'nestSnaplet' or 'embedSnaplet' from
-- within an initializer.


-- $writingSnaplets
-- When writing a snaplet, you must define an initializer function.  The
-- initializer function for the Foo snaplet (where Foo is the snaplet's
-- state type) must have a return type of @Initializer b Foo Foo@.
-- To create an initializer like this, you have to use the 'makeSnaplet'
-- function.  It takes care of the necessary internal bookkeeping needed when
-- initializing a new snaplet.  Haskell's strong type system allows us to
-- ensure that calling 'makeSnaplet' is the only way you can construct a
-- Snaplet type.


-- $routes
-- Snaplet initializers are also responsible for setting up any routes defined
-- by the snaplet.  To do that you'll usually use either 'addRoutes' or
-- 'wrapSite'.


{-


/FIXME/: finish this section


Discuss:

  * lenses and how snaplet apps are built out of parts.

  * the initializer type, and what you can do with it

  * layout of snaplets on disk, and how on-disk stuff can be auto-populated
    from the cabal data directory

  * the handler type, and what you can do with it




{FIXME: strike/rewrite these sentences. Components that do not need any kind
of state or environment are probably more appropriate as a standalone
library than as a snaplet.

We start our application by defining a data structure to hold the state.
This data structure includes the state of any snaplets (wrapped in a
Snaplet) we want to use as well as any other state we might want.}

> module MyApp where
> import Control.Lens
> import Snap.Snaplet
> import Snap.Snaplet.Heist
>
> data App = App
>     { _heist       :: Snaplet (Heist App)
>     , _foo         :: Snaplet Foo
>     , _bar         :: Snaplet Bar
>     , _companyName :: String
>     }
>
> makeLenses ''App

The next thing we need to do is define an initializer.

> app :: Initializer App App App
> app = do
>     hs <- nestSnaplet "heist" $ heistInit "templates"
>     fs <- nestSnaplet "foo" $ fooInit heist
>     bs <- nestSnaplet "" $ nameSnaplet "baz" $ barInit heist
>     addRoutes [ ("/hello", writeText "hello world")
>               ]
>     wrapSite (<|> with heist heistServe)
>     return $ App hs fs bs "fooCorp"

Then we define a simple main to run the application.

> main = serveSnaplet defaultConfig app



Snaplet filesystem directory structure:

> snaplet
>   |-- snaplet.cabal
>   |-- log/
>   |-- src/
> ------------------------
>   |-- db.cfg
>   |-- snaplet.cfg
>   |-- public/
>       |-- stylesheets/
>       |-- images/
>       |-- js/
>   |-- snaplets
>       |-- subsnaplet1/
>       |-- subsnaplet2/
>   |-- templates/

-}

