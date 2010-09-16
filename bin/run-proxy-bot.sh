#!/bin/sh
#
# Pass the path to this script to an engine (e.g. PlayGame.jar) as the
# player argument. It loads the proxy bot system and calls
# PW-PROXY-BOT:PROXY. Don't forget to start a server to connect to.
BASE=`dirname "$0"`

"$BASE"/run-sbcl.sh --load "$BASE/../ProxyBot.lisp"
