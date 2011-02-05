#!/bin/sh

set -x

HADDOCK_OPTS='--html-location=http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html --css=extra/haddock.css'

cabal haddock $HADDOCK_OPTS --hyperlink-source $@

cp extra/logo.gif dist/doc/html/snap/haskell_icon.gif
cp extra/hscolour.css dist/doc/html/snap/src/
