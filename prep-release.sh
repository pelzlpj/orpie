#!/bin/bash
# 'prep-release' script
# Make all the last-minute changes to prepare the sources for packaging
# in a release tarball

echo "Loading source dependencies..."
tla build-config dist.arch
echo "Removing {arch}/ ..."
rm -rf "{arch}"
echo "Generating ./configure ..."
autoconf && rm -rf autom4te.cache
echo "Creating documentation..."
cd doc && make &> /dev/null
echo "Done."




# arch-tag: DO_NOT_CHANGE_f08da183-17c9-42c8-904b-7d6bab8dfe50 
