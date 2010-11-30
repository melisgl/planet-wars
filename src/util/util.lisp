(in-package :pw-util)

(defparameter *verbose* nil
  "Debugging Switch. Set this to T if you want debugging output
written to sbcl.log and for the LOGMSG function to actually do
something. LOGMSG always appends lines to the log so you can just keep
a 'tail -f sbcl.log' running. Set to NIL when submitting!")

(defparameter *log-filename* "sbcl.log")

(defun logmsg (control-string &rest args)
  (when *verbose*
    (with-open-file (stream *log-filename* :direction :output
                     :if-does-not-exist :create
                     :if-exists :append)
      (apply #'format stream control-string args))
    (when (find-package :swank)
      (apply #'format *trace-output* control-string args))))

(defun current-date-time-string ()
  (multiple-value-bind (sec min hou day mon yea)
      (decode-universal-time (get-universal-time))
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            yea mon day hou min sec)))

(defmacro with-reckless-exit (&body body)
  `(unwind-protect
        (progn ,@body)
     ;; On a normal exit or a simple (QUIT) standard output streams are
     ;; flushed. Flushing the streams may error in a pipe. The default
     ;; signal handlers call (QUIT) that throws.
     (sb-ext:quit :recklessly-p t)))

(defmacro with-errors-logged ((&key exit-on-error-p) &body body)
  `(handler-bind ((error
                   (lambda (e)
                     (unless (find-package :swank)
                       (format *error-output*
                               "ERROR: ~A~%~A~%"
                               e
                               (with-output-to-string (s)
                                 (sb-debug:backtrace most-positive-fixnum s)))
                       (let ((*verbose* t))
                         (pw-util:logmsg
                          "ERROR: ~A~%~A~%"
                          e
                          (with-output-to-string (s)
                            (sb-debug:backtrace most-positive-fixnum s)))))
                     (when ,exit-on-error-p
                       (sb-ext:quit :recklessly-p t)))))
     ,@body))

(defun fraction (x)
  (nth-value 1 (round x)))
