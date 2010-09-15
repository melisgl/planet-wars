;;;; -*- mode: Lisp -*-

(asdf:defsystem #:proxy-bot
  :name "Proxy Bot for Planet Wars"
  :author "Gabor Melis"
  :version "0.0.0"
  :licence "MIT"
  :components ((:module "src"
                :serial t
                :components ((:module "proxy-bot"
                              :components ((:file "package")
                                           (:file "proxy-bot"))
                              :serial t))))
  :depends-on (#:planet-wars-util #:sb-bsd-sockets)
  :serial t)
