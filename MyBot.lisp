;;;; This file must be here because the contest server compile
;;;; environment relies on it to recognize that it is a Common Lisp
;;;; submission.

(in-package :cl-user)

(require :asdf)

;;; Any output to stderr makes the compile daemon on the server think
;;; that compilation failed.
(let ((*error-output* *standard-output*))
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
    (asdf:oos 'asdf:load-op :planet-wars)))

(defun parse-config-line (line)
  (let ((pos (position #\= line)))
    (values (subseq line 0 pos)
            (subseq line (1+ pos)))))

(defun path-to-lisp ()
  (with-open-file (stream (merge-pathnames "config" *load-truename*))
    (read-line stream nil nil)
    (nth-value 1 (parse-config-line (read-line stream nil nil)))))

(defun dump (name)
  #+allegro
  (let ((image (format nil "~A.dxl" name)))
    (excl:dumplisp :name image :suppress-allegro-cl-banner t)
    (with-open-file (stream name :direction :output :if-exists :supersede)
      (format stream "#!/bin/sh
~A -I \"~A\" -e '(pwbot::main)'~%" (path-to-lisp) image))
    (excl:exit))
  #+sbcl
  (save-lisp-and-die name :executable t :toplevel #'pwbot::main))
