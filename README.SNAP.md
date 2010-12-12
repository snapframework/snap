Snap Framework
--------------

Snap is a simple and fast web development framework and server written in
Haskell. For more information or to download the latest version, you can visit
the Snap project website at http://snapframework.com/.


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

Snap is currently only officially supported on Unix platforms; it has been
tested on Linux and Mac OSX Snow Leopard, and is reported to work on Windows.


Snap Philosophy
---------------

Snap aims to be the *de facto* web toolkit for Haskell, on the basis of:

  * High performance

  * High design standards

  * Simplicity and ease of use, even for Haskell beginners

  * Excellent documentation

  * Robustness and high test coverage
