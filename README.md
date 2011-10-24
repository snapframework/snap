Snap Framework
==============

Snap is a web framework for Haskell, based on iteratee I/O (as [popularized by
Oleg Kiselyov](http://okmij.org/ftp/Streams.html#iteratee)).  For more
information about Snap, read the `README.SNAP.md` or visit the Snap project
website at http://www.snapframework.com/.

## Library contents

This is top-level project for the Snap Framework, which contains:

  * a command-line utility for creating initial Snap applications

  * a library allowing Snap applications to recompile actions on the
    fly in development mode, with no performance loss in production
    mode.

  * a "snaplet" API allowing web applications to be build from composable
    pieces.

Building snap
=============

The snap tool and library are built using
[Cabal](http://www.haskell.org/cabal/) and
[Hackage](http://hackage.haskell.org/packages/hackage.html). Just run

    cabal install

from the `snap` toplevel directory.


## Building the Haddock Documentation

The haddock documentation can be built using 'cabal haddock'.

The docs get put in `dist/doc/html/`.


## Building the testsuite

To build the test suite, `cd` into the `test/` directory and run

    $ cabal configure
    $ cabal build

From here you can invoke the testsuite by running:

    $ ./runTestsAndCoverage.sh


The testsuite generates an `hpc` test coverage report in `test/dist/hpc`.


## Roadmap to Understanding Snaplets

1. Read Tutorial.lhs which is in `project_template/tutorial/src`.
2. Generate and read the haddock docs.
3. The test code has the nice property that it actually functions as a pretty good example app and covers a lot of the use cases.
4. If you're interested in the implementation, read design.md.

