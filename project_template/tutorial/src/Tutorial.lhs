What Are Snaplets?
==================

A snaplet is a composable web application.  Snaplets allow you to build
self-contained pieces of functionality and glue them together to make larger
applications.  Here are some of the things provided by the snaplet API:

  - Infrastructure for application state/environment

  - Snaplet initialization, reload, and cleanup

  - Management of filesystem data and automatic snaplet installation

  - Unified config file infrastructure

One example might be a wiki snaplet.  It would be distributed as a haskell
package that would be installed with cabal and would probably include code,
config files, HTML templates, stylesheets, JavaScript, images, etc.  The
snaplet's code would provide the necessary API to let your application
interact seamlessly with the wiki functionality.  When you run your
application for the first time, all of the wiki snaplet's filesystem resources
will automatically be copied into the appropriate places.  Then you will
immediately be able to customize the wiki to fit your needs by editing config
files, providing your own stylesheets, etc.  We will discuss this in more
detail later.

A snaplet can represent anything from backend Haskell infrastructure with no
user facing functionality to a small widget like a chat box that goes in the
corner of a web page to an entire standalone website like a blog or forum.
The possibilities are endless.  A snaplet is a web application, and web
applications are snaplets.  This means that using snaplets and writing
snaplets are almost the same thing, and it's trivial to drop a whole website
into another one.

We're really excited about the possibilities available with snaplets.  In
fact, Snap already ships with snaplets for sessions, authentication, and
templating (with Heist),  This gives you useful functionality out of the box,
and jump starts your own snaplet development by demonstrating some useful
design patterns.  So without further ado, let's get started.

Snaplet Overview
================

The heart of the snaplets infrastructure is state management.  Most nontrivial
pieces of a web app need some kind of state or environment data.  Components
that do not need any kind of state or environment are probably more
appropriate as a standalone library than as a snaplet.

Before we continue, we must clarify an important point.  The Snap web server
processes each request in its own green thread.  This means that each request
will receive a separate copy of the state defined by your application and
snaplets, and modifications to that state only affect the local thread that
generates a single response.  From now on, when we talk about state this is
what we are talking about.  If you need global application state, you have to
use a thread-safe construct such as an MVar or IORef.

This post is written in literate Haskell, so first we need to get imports out
of the way.

> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE OverloadedStrings #-}
> 
> module Main where
> 
> import           Data.IORef
> import qualified Data.ByteString.Char8 as B
> import           Data.Maybe
> import           Snap
> import           Snap.Snaplet.Heist
> import           Part2

We start our application by defining a data structure to hold the state.  This
data structure includes the state of all snaplets (wrapped in a Snaplet) used
by our application as well as any other state we might want.

> data App = App
>     { _heist       :: Snaplet (Heist App)
>     , _foo         :: Snaplet Foo
>     , _bar         :: Snaplet Bar
>     , _companyName :: IORef B.ByteString
>     }
>
> makeLenses [''App]

The field names begin with an underscore because of some more complicated
things going on under the hood.  However, all you need to know right now is
that you should prefix things with an underscore and then call `makeLenses`.
This lets you use the names without an underscore in the rest of your
application.

The next thing we need to do is define an initializer.

> appInit :: SnapletInit App App
> appInit = makeSnaplet "myapp" "My example application" Nothing $ do
>     hs <- nestSnaplet "heist" heist $ heistInit "templates"
>     fs <- nestSnaplet "foo" foo $ fooInit
>     bs <- nestSnaplet "" bar $ nameSnaplet "newname" $ barInit foo
>     addRoutes [ ("/hello", writeText "hello world")
>               , ("/fooname", with foo namePage)
>               , ("/barname", with bar namePage)
>               , ("/company", companyHandler)
>               ]
>     wrapHandlers (<|> heistServe)
>     ref <- liftIO $ newIORef "fooCorp"
>     return $ App hs fs bs ref

For now don't worry about all the details of this code.  We'll work through the
individual pieces one at a time.  The basic idea here is that to initialize an
application, we first initialize each of the snaplets, add some routes, run a
function wrapping all the routes, and return the resulting state data
structure.  This example demonstrates the use of a few of the most common
snaplet functions.

nestSnaplet
-----------
   
All calls to child snaplet initializer functions must be wrapped in a call to
nestSnaplet.  The first parameter is a URL path segment that is used to prefix
all routes defined by the snaplet.  This lets you ensure that there will be no
problems with duplicate routes defined in different snaplets.  If the foo
snaplet defines a route `/foopage`, then in the above example, that page will
be available at `/foo/foopage`.  Sometimes though, you might want a snaplet's
routes to be available at the top level.  To do that, just pass an empty string
to nestSnaplet as shown above with the bar snaplet.

In our example above, the bar snaplet does something that needs to know about
the foo snaplet.  Maybe foo is a database snaplet and bar wants to store or
read something.  In order to make that happen, it needs to have a "handle" to
the snaplet.  Our handles are whatever field names we used in the App data
structure minus the initial underscore character.  They are automatically
generated by the `makeLenses` function.  For now it's sufficient to think of
them as a getter and a setter combined (to use an OO metaphor).

The second parameter to nestSnaplet is the lens to the snaplet you're nesting.
In order to place a piece into the puzzle, you need to know where it goes.

nameSnaplet
-----------

The author of a snaplet defines a default name for the snaplet in the first
argument to the makeSnaplet function.  This name is used for the snaplet's
directory in the filesystem.  If you don't want to use the default name, you
can override it with the `nameSnaplet` function.  Also, if you want to have two
instances of the same snaplet, then you will need to use `nameSnaplet` to give
at least one of them a unique name.

addRoutes
---------

The `addRoutes` function is how an application (or snaplet) defines its
routes.  Under the hood the snaplet infrastructure merges all the routes from
all snaplets, prepends prefixes from `nestSnaplet` calls, and passes the list
to Snap's
[route](http://hackage.haskell.org/packages/archive/snap-core/0.5.1.4/doc/html/Snap-Types.html#v:route)
function.

A route is a tuple of a URL and a handler function that will be called when
the URL is requested.  Handler is a wrapper around the Snap monad that handles
the snaplet's infrastructure.  During initialization, snaplets use the
`Initializer` monad.  During runtime, they use the `Handler` monad.  We'll
discuss `Handler` in more detail later.  If you're familiar with Snap's old
extension system, you can think of it as roughly equivalent to the Application
monad.  It has a `MonadState` instance that lets you access and modify the
current snaplet's state, and a `MonadSnap` instance providing the
request-processing functions defined in Snap.Types.

wrapHandlers
------------

`wrapHandlers` allows you to apply an arbitrary `Handler` transformation to
the top-level handler.  This is useful if you want to do some generic
processing at the beginning or end of every request.  For instance, a session
snaplet might use it to touch a session activity token before routing happens.
It could also be used to implement custom logging.  The example above uses it
to define heistServe (provided by the Heist snaplet) as the default handler to
be tried if no other handler matched.  This may seem like an easy way to define
routes, but if you string them all together in this way each handler will be
evaluated sequentially and you'll get O(n) time complexity, whereas routes
defined with `addRoutes` have O(log n) time complexity.  Therefore, in a
real-world application you would probably want to have `("", heistServe)` in
the list passed to `addRoutes`.

with
----

The last unfamiliar function in the example is `with`.  Here it accompanies a
call to the function `namePage`.  `namePage` is a simple example handler and
looks like this.

> namePage :: Handler b v ()
> namePage = do
>     mname <- getSnapletName
>     writeText $ fromMaybe "This shouldn't happen" mname

This function is a generic handler that gets the name of the current snaplet
and writes it into the response with the `writeText` function defined by the
snap-core project.  The type variables 'b' and 'v' indicate that this function
will work in any snaplet with any base application.  The 'with' function is
used to run `namePage` in the context of the snaplets foo and bar for the
corresponding routes.  

Site Reloading
--------------

Snaplet Initializers serve dual purpose as both initializers and reloaders.
Reloads are triggered by a special handler that is bound to the
`/admin/reload` route.  This handler re-runs the site initializer and if it is
successful, loads the newly generated in-memory state.  To prevent denial of
service attacks, the reload route is only accessible from localhost.

If there are any errors during reload, you would naturally want to see them in
the HTTP response returned by the server.  However, when these same
initializers are run when you first start your app, you will want to see
status messages printed to the console.  To make this possible we provide the
`printInfo` function.  You should use it to output any informational messages
generated by your initializers.  If you print directly to standard output or
standard error, then those messages will not be available in your browser when
you reload the site.

Working with state
------------------

`Handler b v` has a `MonadState v` instance.  This means that you can access
all your snaplet state through the get, put, gets, and modify functions that
are probably familiar from the state monad.  In our example application we
demonstrate this with `companyHandler`.

> companyHandler :: Handler App App ()
> companyHandler = method GET getter <|> method POST setter
>   where
>     getter = do
>         nameRef <- gets _companyName
>         name <- liftIO $ readIORef nameRef
>         writeBS name
>     setter = do
>         mname <- getParam "name"
>         nameRef <- gets _companyName
>         liftIO $ maybe (return ()) (writeIORef nameRef) mname
>         getter

If you set a GET request to `/company`, you'll get the string "fooCorp" back.
If you send a POST request, it will set the IORef held in the `_companyName`
field in the `App` data structure to the value of the `name` field.  Then it
calls the getter to return that value back to you so you can see it was
actually changed.  Again, remember that this change only persists across
requests because we used an IORef.  If `_companyName` was just a plain string
and we had used modify, the changed result would only be visible in the rest
of the processing for that request.

The Heist Snaplet
=================

The astute reader might ask why there is no `with heist` in front of the call
to `heistServe`.  And indeed, that would normally be the case.  But we decided
that an application will never need more than one instance of a Heist snaplet.
So we provided a type class called `HasHeist` that allows an application to
define the global reference to its Heist snaplet by writing a `HasHeist`
instance.  In this example we define the instance as follows:

> instance HasHeist App where heistLens = subSnaplet heist

Now all we need is a simple main function to serve our application.

> main :: IO ()
> main = serveSnaplet defaultConfig appInit

This completes a full working application.  We did leave out a little dummy
code for the Foo and Bar snaplets.  This code is included in Part2.hs.  For
more information look in our [API
documentation](http://hackage.haskell.org/packages/archive/snap/0.6.0.2/doc/html/Snap-Snaplet.html).
No really, that wasn't a joke.  The API docs are written as prose.  It is
written to be very easy to read, while having the benefit of including all the
actual type signatures.

Filesystem Data and Automatic Installation
==========================================

Some snaplets will have data stored in the filesystem that should be installed
into the directory of any project that uses it.  Here's an example of what a
snaplet filesystem layout might look like:

    foosnaplet/
      |-- *snaplet.cfg*
      |-- db.cfg
      |-- public/
          |-- stylesheets/
          |-- images/
          |-- js/
      |-- *snaplets/*
          |-- *heist/*
              |-- templates/
          |-- subsnaplet1/
          |-- subsnaplet2/

Only the starred items are actually enforced by current code, but we want to
establish the others as a convention.  The file snaplet.cfg is automatically
read by the snaplet infrastructure.  It is available to you via the
`getSnapletUserConfig` function.  Config files use the format defined by Bryan
O'Sullivan's excellent [configurator
package](http://hackage.haskell.org/package/configurator).  In this example,
the user has chosen to put db config items in a separate file and use
configurator's import functionality to include it in snaplet.cfg.  If
foosnaplet uses `nestSnaplet` or `embedSnaplet` to include any other snaplets,
then filesystem data defined by those snaplets will be included in
subdirectories under the `snaplets/` directory.

So how do you tell the snaplet infrastructure that your snaplet has filesystem
data that should be installed?  Look at the definition of appInit above.  The
third argument to the makeSnaplet function is where we specify the filesystem
directory that should be installed.  That argument has the type `Maybe (IO
FilePath)`.  In this case we used `Nothing` because our simple example doesn't
have any filesystem data.  As an example, let's say you are creating a snaplet
called killerapp that will be distributed as a hackage project called
snaplet-killerapp.  Your project directory structure will look something like
this:

    snaplet-killerapp/
      |-- resources/
      |-- snaplet-killerapp.cabal
      |-- src/

All of the files and directories listed above under foosnaplet/ will be in
resources/.  Somewhere in the code you will define an initializer for the
snaplet that will look like this:

    killerInit = makeSnaplet "killerapp" "42" (Just dataDir) $ do

The primary function of Cabal is to install code.  But it has the ability to
install data files and provides a function called `getDataDir` for retrieving
the location of these files.  Since it returns a different result depending on
what machine you're using, the third argument to `makeSnaplet` has to be `Maybe
(IO FilePath)` instead of the more natural pure version.  To make things more
organized, we use the convention of putting all your snaplet's data files in a
subdirectory called resources.  So we need to create a small function that
appends `/resources` to the result of `getDataDir`.

    import Paths_snaplet_killerapp
    dataDir = liftM (++"/resources") getDataDir

If our project is named snaplet-killerapp, the `getDataDir` function is
defined in the module Paths_snaplet_killerapp, which we have to import.  To
make everything work, you have to tell Cabal about your data files by
including a section like the following in snaplet-killerapp.cabal:

    data-files:
      resources/snaplet.cfg,
      resources/public/stylesheets/style.css,
      resources/templates/page.tpl

Now whenever your snaplet is used, its filesystem data will be automagically
copied into the local project that is using it, whenever the application is
run and it sees that the files don't already exist.

