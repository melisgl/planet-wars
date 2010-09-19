(defpackage :planet-wars
  (:nicknames :pw :pwbot)
  (:use :cl :sb-bsd-sockets)
  (:export #:play
           #:start-server-for-proxy-bot))
