#!/bin/sh
sbcl --dynamic-space-size 700 --noinform --end-runtime-options \
    --noprint --no-userinit --no-sysinit --disable-debugger \
    "$@"
