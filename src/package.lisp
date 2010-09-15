(defpackage :planet-wars
  (:nicknames :pw)
  (:use :cl :sb-bsd-sockets)
  (:export #:play
           #:start-server-for-proxy-bot))
