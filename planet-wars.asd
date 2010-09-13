;;;; -*- mode: Lisp -*-

(asdf:defsystem #:planet-wars
  :name "Planet Wars Bot for the Google AI contest"
  :author "Gabor Melis"
  :version "0.0.0"
  :licence "MIT"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "model")
                             (:file "io")
                             (:file "play")
                             (:file "engine"))))
  :depends-on (#:parse-number #:split-sequence)
  :serial t)
