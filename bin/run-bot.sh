#!/bin/sh
#
# Pass the path to this script to an engine (e.g. PlayGame.jar) as the
# player argument. It loads the planet-wars system and calls PW:PLAY.
BASE=`dirname "$0"`

"$BASE"/run-sbcl.sh --load "$BASE/../MyBot.lisp" --eval '(pw:play)'
