(defpackage :planet-wars
  (:nicknames :pw :pwbot)
  (:use :cl #+sbcl :sb-bsd-sockets :pw-util)
  (:export #:play
           #:start-server-for-proxy-bot))
