;;;; -*- mode: Lisp -*-

(asdf:defsystem #:planet-wars
  :name "Planet Wars Bot for the Google AI contest"
  :author "Gabor Melis"
  :version "0.83"
  :licence "MIT"
  :components ((:module "src"
                :serial t
                :components (#+sbcl (:file "timer")
                             (:file "package")
                             (:file "model")
                             (:file "io")
                             (:file "play")
                             (:file "player")
                             (:file "alpha-beta"))))
  :depends-on (#:parse-number #:split-sequence #:usocket
                              #:alexandria #:planet-wars-util)
  :serial t)
