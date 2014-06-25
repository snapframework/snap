#!/bin/sh

set -e

if [ -z "$DEBUG" ]; then
    export DEBUG=snap-testsuite
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
Paths_snap
Snap.Snaplet.Auth.App
Snap.Snaplet.Auth.Handlers.Tests
Snap.Snaplet.Auth.Tests
Snap.Snaplet.Heist.App
Snap.Snaplet.Heist.Tests
Snap.Snaplet.Internal.Lensed.Tests
Snap.Snaplet.Internal.LensT.Tests
Snap.Snaplet.Internal.RST.Tests
Snap.Snaplet.Internal.Tests
Snap.TestCommon
Snap.Snaplet.Test.App
Snap.Snaplet.Test.Tests
Snap.Snaplet.Auth.SpliceTests
Snap.Snaplet.Auth.Types.Tests
Snap.Snaplet.Config.App
Snap.Snaplet.Config.Tests
'


EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

rm -f non-cabal-appdir/snaplets/heist/templates/bad.tpl
rm -f non-cabal-appdir/snaplets/heist/templates/good.tpl
rm -fr non-cabal-appdir/snaplets/foosnaplet

hpc markup $EXCL --destdir=$DIR testsuite # >/dev/null 2>&1

cat <<EOF

Test coverage report written to $DIR.
EOF
