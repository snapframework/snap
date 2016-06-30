#! /bin/sh

# This script is a convenient shortcut for pulling and updating all
# submodules.

# The following gets the versions parent (snap) is pointing, not necessaraly the
# latest.
git pull && git submodule update --init --recursive

# This will get the latest. Use with care..
git submodule foreach "(git checkout master; git pull --recurse-submodules)&"

# This will show, what has changed
# git status

# Haven't tried the following:
# git submodule init
# git submodule update
# git submodule foreach 'git fetch origin; git checkout $(git rev-parse --abbrev-ref HEAD); git reset --hard origin/$(git rev-parse --abbrev-ref HEAD); git submodule update --recursive; git clean -dfx'

