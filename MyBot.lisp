;;;; This file must be here because the contest server compile
;;;; environment relies on it to recognize that it is a Common Lisp
;;;; submission.

;;; Any output to stderr makes the compile daemon on the server think
;;; that compilation failed.
(handler-bind ((error
                (lambda (c)
                  (declare (ignore c))
                  (format *standard-output*
                          "System info:~% ~S~%"
                          (list *default-pathname-defaults*
                                *features*
                                (lisp-implementation-type)
                                (lisp-implementation-version)
                                (directory "**"))))))
  (load (merge-pathnames "setup.lisp" *load-truename*))
  (require :planet-wars))
