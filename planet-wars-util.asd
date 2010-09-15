;;;; -*- mode: Lisp -*-

(asdf:defsystem #:planet-wars-util
  :name "Utilities for Planet Wars"
  :description "Common code for the proxy and the normal bot."
  :author "Gabor Melis"
  :version "0.0.0"
  :licence "MIT"
  :components ((:module "src"
                :serial t
                :components ((:module "util"
                              :components ((:file "package")
                                           (:file "util"))
                              :serial t))))
  :serial t)
