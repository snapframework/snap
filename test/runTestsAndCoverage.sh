#!/bin/sh

set -e

if [ -z "$DEBUG" ]; then
    export DEBUG=testsuite
fi

SUITE=./dist/build/testsuite/testsuite

rm -f testsuite.tix

if [ ! -f $SUITE ]; then
    cat <<EOF
Testsuite executable not found, please run:
    cabal configure
then
    cabal build
EOF
    exit;
fi

$SUITE $*

DIR=dist/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES='Main
Blackbox.App
Blackbox.BarSnaplet
Blackbox.Common
Blackbox.EmbeddedSnaplet
Blackbox.FooSnaplet
Blackbox.Tests
Blackbox.Types
Snap.Snaplet.Internal.Lensed.Tests
Snap.Snaplet.Internal.LensT.Tests
Snap.Snaplet.Internal.RST.Tests
Snap.Snaplet.Internal.Tests
'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

mv non-cabal-appdir/testsuite.tix .
rm -f non-cabal-appdir/templates/bad.tpl
rm -f non-cabal-appdir/templates/good.tpl
rm -fr non-cabal-appdir/snaplets/foosnaplet

hpc markup $EXCL --destdir=$DIR testsuite >/dev/null 2>&1

cat <<EOF

Test coverage report written to $DIR.
EOF
