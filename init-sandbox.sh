#! /bin/sh

cabal sandbox init
cabal sandbox add-source deps/io-streams
cabal sandbox add-source deps/io-streams-haproxy
cabal sandbox add-source deps/snap-core
cabal sandbox add-source deps/snap-server
cabal sandbox add-source deps/xmlhtml
cabal sandbox add-source deps/heist
