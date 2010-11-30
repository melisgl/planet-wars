(defpackage :planet-wars
  (:nicknames :pw :pwbot)
  (:use :cl :sb-bsd-sockets :pw-util)
  (:export #:play
           #:start-server-for-proxy-bot))
