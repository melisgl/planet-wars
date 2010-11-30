#!/bin/sh
#
# Don't run this by hand, use 'make submission'.

GIT_VERSION=`git describe --tags --dirty`
echo "$GIT_VERSION" > version
DIR="$1"
ZIP="$2"
cd ..
find "$DIR" -name '*.lisp' -o -name '*.asd' -o -name version \
    | xargs zip -r "$DIR.zip"
