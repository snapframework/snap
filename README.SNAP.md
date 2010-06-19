Snap Framework
--------------

This is the first developer prerelease of the Snap framework.  Snap is a simple
and fast web development framework and server written in Haskell. For more
information or to download the latest version, you can visit the Snap project
website at http://snapframework.com/.


Snap Status and Features
------------------------

This developer prerelease contains only the Snap core system, namely:

  * a high-speed HTTP server, with an optional high-concurrency backend using
    the [libev](http://software.schmorp.de/pkg/libev.html) library

  * a sensible and clean monad for web programming

  * an xml-based templating system for generating HTML based on
    [expat](http://expat.sourceforge.net/) (via
    [hexpat](http://hackage.haskell.org/package/hexpat)) that allows you to
    bind Haskell functionality to XML tags without getting PHP-style tag soup
    all over your pants

Snap currently only runs on Unix platforms; it has been tested on Linux and Mac
OSX Snow Leopard.


Snap Philosophy
---------------

Snap aims to be the *de facto* web toolkit for Haskell, on the basis of:

  * High performance

  * High design standards

  * Simplicity and ease of use, even for Haskell beginners

  * Excellent documentation

  * Robustness and high test coverage


Snap Roadmap
------------

Where are we going?

1. First prerelease: HTTP server, monad, template system

2. Second prerelease: component system with a collection of useful stock
modules (called "Snaplets") for things like user and session management,
caching, an administrative interface, etc.
