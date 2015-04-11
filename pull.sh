#! /bin/sh

# This script is a convenient shortcut for pulling and updating all
# submodules.

git pull && git submodule update --init --recursive
