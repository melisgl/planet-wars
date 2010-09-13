(in-package :planet-wars)

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
      (apply #'format stream control-string args))))

(defun current-date-time-string ()
  (multiple-value-bind (sec min hou day mon yea)
      (decode-universal-time (get-universal-time))
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            yea mon day hou min sec)))


;;;; Interface for players

(defgeneric parse-game (player stream))

(defgeneric compute-orders (player))

(defgeneric game (player))


(defun main (&key
             (player (make-instance 'iteratively-deepening-distance-player))
             (input *standard-input*) (output *standard-output*))
  (logmsg "~&~%=== New game: ~A ===~%" (current-date-time-string))
  (handler-bind ((error
                  (lambda (e)
                    (logmsg "ERROR: ~A~%~A~%"
                            e
                            (with-output-to-string (s)
                              (sb-debug:backtrace most-positive-fixnum s)))
                    (sb-ext:quit :recklessly-p t))))
    (loop while (peek-char nil input nil nil)
          for turn from 0 do
          (logmsg "* turn ~A~%" turn)
          (logmsg "~A~%"
                  (with-output-to-string (*trace-output*)
                    (time
                     (progn
                       (parse-game player input)
                       (let ((orders (compute-orders (game player))))
                         (logmsg "* orders~%~S~%" orders)
                         (write-orders orders output))
                       (write-line "go" output))))))
    ;; Sometimes necessary because output streams can be closed and
    ;; UNIX-EXIT runs into an error.
    (sb-ext:quit :recklessly-p t)))
