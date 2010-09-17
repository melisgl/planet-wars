#!/bin/sh
BASE=`dirname "$0"`
cd "$BASE"
sh ./run-sbcl.sh --load ../setup.lisp \
    --eval "(require :planet-wars)" \
    --eval "(save-lisp-and-die \"../MyBot\" :executable t :toplevel #'planet-wars:play)"
