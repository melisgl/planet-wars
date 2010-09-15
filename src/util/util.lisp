(in-package :pw-util)

(defparameter *verbose* t
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
      (apply #'format stream control-string args))))

(defun current-date-time-string ()
  (multiple-value-bind (sec min hou day mon yea)
      (decode-universal-time (get-universal-time))
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            yea mon day hou min sec)))

