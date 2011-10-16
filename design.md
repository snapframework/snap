# Snaplet Design

The Snaplet infrastructure was designed with three high-level design goals:

* Request local state
* Composability
* Availability

First, request local state means that snaplets should be able to define their
own state that will be available during request processing.  And that state
should be mutable with scope local to the request.

Composability means that applications and snaplets should be interchangeable,
and you should be able to build them by gluing together other snaplets.

Availability means that you should be able to access your application state
without threading it manually through parameters.

## Handler

Implementing the goal of request local state means that we need some kind of a
Handler monad that will look roughly like a state transformer built on top of
the Snap monad with the top level application data as the state.  To implement
composability we also need an additional type parameter that can be changed to
match the scope of the current snaplet.  We use the `withReader :: (r1 -> r2)
-> Reader r2 a -> Reader r1 a` pattern to manage scope changes, but in order
to make our state composably mutable, we need to enlist the help of lenses
instead of accessor functions.  This allows us to keep only the top level
state and mutate the current context using the lens.

The LensT monad is our implementation of this abstraction.  It is a
combination of ReaderT and StateT (our RST abstraction).  Since the lens is
not conceptually mutable in the same way as the actual state, it is stored in
the reader environment.  The state monad part is used for the top level state
b, giving is the following newtype.

    newtype LensT b v s m a = LensT (RST (Lens b v) s m a)

LensT comes with a (MonadReader (Lens b v)) instance for retrieving the lens
and a (MonadState v) instance that uses the lens transparently to achieve
stateful behavior with the type v.  From here the definition of Handler is
fairly natural:

    newtype Handler b v a =
        Handler (LensT (Snaplet b) (Snaplet v) (Snaplet b) Snap a)

We use `LensT (Snaplet b) (Snaplet v)` instead of `LensT b (Snaplet v)`
because it is desirable to be able to use the identity lens to construct a
`Handler b b`.  The only issue with this formulation is that the lens
manipulation functions provided by LensT are not what the end user needs.  The
end user has a lens of type `Lens b (Snaplet v)` created by the `mkLabels`
function.  But LensT's withXYZ functions need `Lens (Snaplet b) (Snaplet v)`
lenses.  These can be derived easily by composing the user-supplied lens with
the internal lens `Lens (Snaplet a) a` derived from the definition of the
Snaplet data structure.

NOTE: The above definition for Handler is no longer correct.  We switched to a
slightly more specialized monad formulation called Lensed that avoids
traversal of the whole state hierarchy when the state is manipulated.  Thanks
to Edward Kmett for pointing this out and writing the code for us.

## Initializer

The second important component of snaplets is initialization.  This involves
setting up the state used by the handlers as well as defining a snaplet's
routes and cleanup actions, reading on-disk config files, and initializing and
interacting with other snaplets.  `Initializer` still uses a LensT
implementation because it does not fit the more specialized case for which
Lensed is optimized.  But it is similar enough that we can still refer to
snaplets using the same lenses that we use in Handlers.  These similarities
are abstracted in the MonadSnaplet type class.

During initialization, sometimes you want to modify the result of another
snaplet's initialization.  For instance, maybe you want to add templates or
bind splices for a sitewide Heist snaplet.  Or perhaps you want to add
controls to the admin panel snaplet.  This involves modifying the state of
other snaplets.  It would be nice to use the same lenses and scoped
modification via top-level state that we use in `Handler`.  But in the
initializer we don't yet have a fully constructed top-level state object to
modify.  So instead of actually modifying the state directly, we construct
modifier functions to be applied at the end of initialization.  Since these
functions form a monoid, we can build them up using WriterT as LensT's
underlying monad.

The `Initializer` monad is used for both initialization and application
reloading.  When an application is reloaded from the browser, status and error
messages should go to the browser instead of the console.  The printInfo
function sends messages to the appropriate place and should be used to
communicate all initializer status and errors.

## Heist

The Heist snaplet is a fairly complex snaplet that illustrates a number of
concepts that you may encounter while writing your own snaplets.  The biggest
issue arises because Heist's TemplateState is parameterized by the handler
monad.  This means that if you want to do something like a with transformation
with a lens `Lens b v` you will naturally want to apply the same
transformation to the Handler parameter of the TemplateState.  Unfortunately,
due to Heist's design, this is computationally intensive, must be performed at
runtime, and requires that you have a bijection between b and v.  To avoid
this issue, we only use the base application state, `TemplateState (Handler b
b)`.

The basic functions for manipulating templates are not affected by this
decision.  But the splice functions are more problematic since they are the
ones that actually use TemplateState's monad parameter.

You will also notice that the Heist snaplet includes a HasHeist type class.
Normally to use snaplets, you must "call" them using with or withTop,
passing the lens to the desired snaplet.  This is useful because it allows you
to have multiple instances of the same snaplet.  However, there may be times
when you know you will only ever need a single instance of a particular
snaplet and you'd like to avoid the need to manually change the context every
time.

This is where type classes are useful.  The HasHeist type class essentially
defines some global compile-time state associating a particular lens to be
used for calls to Heist within a particular type.  To use Heist, just define a
HasHeist instance for your application or snaplet type and all the Heist API
functions will work without needing with.  Your HasHeist instance will
look something like this:

    instance HasHeist App where
        heistLens = subSnaplet heist

The call to subSnaplet is required because HasHeist needs a `Lens
(Snaplet v) (Snaplet (Heist b))` instead of the lens `Lens v (Snaplet (Heist
b))` that you willll get from mkLabels.

