#!/bin/sh

set -e

# All directory variables relative to project root
DIR=dist-newstyle/hpc

SUITE=./dist-newstyle/build/snap-1.0.0.0/build/testsuite/testsuite

if [ -z "$DEBUG" ]; then
    export DEBUG=snap-testsuite
fi

rm -f testsuite.tix
rm -rf "$DIR"
mkdir -p "$DIR"

if [ ! -f $SUITE ]; then
    cat <<EOF
Testsuite executable not found, please run:
    cabal install --enable-tests --only-dependencies
    cabal configure --enable-tests
    cabal build
EOF
    exit;
fi

$SUITE $*

EXCLUDES='Main
Snap
Blackbox.App
Blackbox.BarSnaplet
Blackbox.Common
Blackbox.EmbeddedSnaplet
Blackbox.FooSnaplet
Blackbox.Tests
Blackbox.Types
Paths_snap
Snap.Snaplet.Auth.Handlers.Tests
Snap.Snaplet.Auth.Tests
Snap.Snaplet.Test.Common.App
Snap.Snaplet.Test.Common.BarSnaplet
Snap.Snaplet.Test.Common.EmbeddedSnaplet
Snap.Snaplet.Test.Common.FooSnaplet
Snap.Snaplet.Test.Common.Handlers
Snap.Snaplet.Test.Common.Types
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

rm -f test/snaplets/heist/templates/bad.tpl
rm -f test/snaplets/heist/templates/good.tpl
rm -fr test/non-cabal-appdir/snaplets/foosnaplet # TODO

# TODO - actually send results to /dev/null when hpc kinks are fully removed
hpc markup $EXCL --destdir=$DIR testsuite # >/dev/null 2>&1

cat <<EOF

Test coverage report written to $HTMLDIR.
EOF
