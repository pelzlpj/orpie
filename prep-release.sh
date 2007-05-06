#!/bin/bash
# 'prep-release' script
# Make all the last-minute changes to prepare the sources for packaging
# in a release tarball
#
# Usage: prep-release.sh DESTDIR
#

echo "Exporting revision..."
bzr export $1
echo "Exporting dependencies..."
bzr export $1/curses $HOME/src/bzr-repo/libcurses-ocaml-dev
bzr export $1/units $HOME/src/bzr-repo/ocaml-units-dev

cd $1
echo "Generating ./configure ..."
autoconf && rm -rf autom4te.cache
echo "Creating documentation..."
cd doc && make &> /dev/null
echo "Done."


