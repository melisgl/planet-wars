#!/bin/sh
BASE=`dirname "$0"`

"$BASE"/run-sbcl.sh --load "$BASE/../MyBot.lisp"
