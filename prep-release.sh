#!/bin/bash
# 'prep-release' script
# Make all the last-minute changes to prepare the sources for packaging
# in a release tarball
#
# Usage: prep-release.sh DESTDIR
#

set -e

echo "Exporting revision..."
mkdir -p $1 && git archive HEAD | tar -C $1 --extract
echo "Exporting dependencies..."
mkdir -p $1/curses && git archive --remote="$HOME/src/ocaml-curses-legacy" HEAD | tar -C $1/curses --extract
mkdir -p $1/units && git archive --remote="$HOME/src/ocaml-units" HEAD | tar -C $1/units --extract

cd $1
echo "Generating ./configure ..."
autoconf && rm -rf autom4te.cache
echo "Creating documentation..."
cd doc && make &> /dev/null
echo "Done."


