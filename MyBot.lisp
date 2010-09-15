;;;; This file must be here because the contest server compile
;;;; environment relies on it.
;;;;
;;;; ASDF on the server gets into infinite recursion so load stuff
;;;; manually.

(defparameter *interesting-debug-info* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-and-load (interesting-debug-info)
    (declare (optimize debug))
    (setq *interesting-debug-info* interesting-debug-info)
    (dolist (name '("parse-number" "split-sequence"
                    "package" "model" "io" "player" "play"))
      (load (compile-file (format nil "src/~A.lisp" name))))))

(compile-and-load (list *default-pathname-defaults*
                        *features*
                        sb-impl::*default-external-format*
                        (directory "**")))
