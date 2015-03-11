Snap Framework
==============

Snap is a simple and fast web development framework and server written in
Haskell. For more information about Snap, read the `README.SNAP.md` or visit
the Snap project website at http://www.snapframework.com/.

## Library contents

This is top-level project for the Snap Framework, which contains:

  * a library allowing Snap applications to recompile actions on the
    fly in development mode, with no performance loss in production
    mode.

  * a "snaplet" API allowing web applications to be build from composable
    pieces.

The command-line utility `snap` for creating initial Snap applications used to
be a part of this package. As of version 1.0, the snap command-line utility is
no longer provided by this package.  It is now provided by the package
[`snap-templates`](https://github.com/snapframework/snap-templates).

Building snap
=============

After you clone the repository, change to the newly created snap directory and
run

    git submodule update --init --recursive

This updates all the Snap Framework dependencies to the correct version.  The
snap library is built using [Cabal](http://www.haskell.org/cabal/) and
[Hackage](http://hackage.haskell.org/packages/hackage.html). Now just run

    cabal install

from the `snap` toplevel directory.


## Building the Haddock Documentation

The haddock documentation can be built using 'cabal haddock'.

The docs get put in `dist/doc/html/`.


## Building the testsuite

To build the test suite, run

    $ cabal clean
    $ cabal configure --enable-tests --enable-library-coverage
    $ cabal build
    $ cabal install --enable-tests

From here you can invoke the testsuite by running:

    $ ./runTestsAndCoverage.sh


The testsuite generates an `hpc` test coverage report in `dist/hpc`.


## Roadmap to Understanding Snaplets

1. Read `Tutorial.lhs` which is in the `project_template/tutorial/src` directory of the `snap-template` package.
2. Generate and read the haddock docs.
3. The test code has the nice property that it actually functions as a pretty good example app and covers a lot of the use cases.
4. If you're interested in the implementation, read design.md.
