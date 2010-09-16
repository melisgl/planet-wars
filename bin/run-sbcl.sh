#!/bin/sh
#
# Wrapper for sbcl. Set common command line arguments to sbcl.

sbcl --dynamic-space-size 768 --noinform --lose-on-corruption \
    --end-runtime-options \
    --noprint --no-userinit --no-sysinit --disable-debugger \
    "$@"
