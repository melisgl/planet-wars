#!/bin/sh
#
# Wrapper for sbcl. Set common command line arguments to sbcl and
# initialize ASDF:*CENTRAL-REGISTRY*.

ASDF_DIRS_FILE=`dirname "$0"`/../.asdf-dirs
ASDF_DIRS=`cat "$ASDF_DIRS_FILE"`

sbcl --dynamic-space-size 896 --noinform --lose-on-corruption \
    --end-runtime-options \
    --noprint --no-userinit --no-sysinit --disable-debugger \
    --eval "(setf asdf:*central-registry* (list $ASDF_DIRS))" \
    "$@"
