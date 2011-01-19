;;;; This file can also be loaded to run a proxy bot without compiling
;;;; and saving an image with `make ProxyBot'.

(in-package :cl-user)

(require :asdf)

;;; Load the sytem, but make sure nothing is written to the orignal
;;; stdout as that's read by the engine.
(let ((*standard-output* *error-output*))
  (load (merge-pathnames "setup.lisp" *load-truename*))
  (asdf:oos 'asdf:load-op :proxy-bot))

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
    (with-open-file (stream (string name) :direction :output
                     :if-exists :supersede)
      (format stream "#!/bin/sh
~A -I \"~A\" -e '(pw-proxy-bot:proxy)'~%" (path-to-lisp) image))
    (excl:exit))
  #+sbcl
  (save-lisp-and-die name :executable t :toplevel #'pw-proxy-bot:proxy))
