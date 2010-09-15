#!/bin/sh
#
# Don't run this by hand, use 'make submission'.

DIR="$1"
ZIP="$2"
cd ..
find "$DIR" -name '*.lisp' -o -name '*.asd'| xargs zip -r "$DIR.zip"
