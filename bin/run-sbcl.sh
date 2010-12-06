#!/bin/sh
#
# Wrapper for sbcl. Set common command line arguments to sbcl.

BASE=`dirname "$0"`

. "$BASE/../config"

"$LISPBIN" --dynamic-space-size 768 --noinform --lose-on-corruption \
    --end-runtime-options \
    --noprint --no-userinit --no-sysinit --disable-debugger \
    "$@"
