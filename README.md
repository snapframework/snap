# Snap Framework [![Hackage Status](https://img.shields.io/hackage/v/snap.svg)](https://hackage.haskell.org/package/snap)

[![GitHub CI](https://github.com/snapframework/snap/workflows/CI/badge.svg)](https://github.com/snapframework/snap/actions)

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

# Building snap

After you clone the repository, change to the newly created snap directory and
run

    git submodule update --init --recursive
    cabal build all

(You may want to look at pull.sh or pullLatestMaster.sh.)
This updates all the Snap Framework dependencies to the correct version.
The snap library is built using
[Cabal](http://www.haskell.org/cabal/) and
[Hackage](http://hackage.haskell.org/packages/hackage.html).

## Building with Nix

A Nix shell is provided and can be entered using `nix-shell`.

If using `nix-direnv`, run the following:

```sh
echo 'use nix' > .envrc && direnv allow
```

## Building the Haddock Documentation

The haddock documentation can either be built for _snap_ using `cabal haddock snap`, or for the git
submodules as well using `cabal haddock-project`.

## Building the testsuite

Build and run the test suite using `cabal test all`.

## Roadmap to Understanding Snaplets

1. Read `Tutorial.lhs` which is in the `project_template/tutorial/src` directory of the `snap-templates` package.
2. Generate and read the haddock docs.
3. The test code has the nice property that it actually functions as a pretty good example app and covers a lot of the use cases.
4. If you're interested in the implementation, read design.md.
