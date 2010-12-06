(in-package :pw-proxy-bot)

(defun connection-refused (&optional arg)
  (declare (ignore arg))
  (pw-util:logmsg "Connection refused. Aborting...~%")
  (pw-util:exit :status 111))

(defun proxy ()
  (pw-util:with-reckless-exit
    (pw-util:with-errors-logged (:exit-on-error-p t)
      (setq pw-util:*verbose* t)
      (setq pw-util:*log-filename* "proxy-bot.log")
      (pw-util:logmsg "* ProxyBot started at ~A~%"
                      (pw-util:current-date-time-string))
      (pw-util:logmsg "Connecting to real bot at 127.0.0.1:41807...~%")
      (handler-bind ((connection-refused-error #'connection-refused))
        (let ((socket (usocket:socket-connect #+sbcl #(127 0 0 1)
                                              #+allegro "localhost"
                                              41807)))
          (handler-bind ((error
                          (lambda (e)
                            (pw-util:logmsg
                             "ERROR: ~A~%~A~%"
                             e
                             (with-output-to-string (s)
                               (pw-util:backtrace-to-stream s)))
                            (pw-util:exit :recklessly-p t))))
            (unwind-protect
                 (loop with stream = (usocket:socket-stream socket)
                       while (peek-char nil *standard-input* nil)
                       for turn from 1
                       do (pw-util:logmsg "** turn: ~S~%" turn)
                       (pw-util:logmsg "Sending game state...~%")
                       (loop for line = (read-line *standard-input* nil)
                             do (write-line line stream)
                             until (equal line "go")
                             finally (force-output stream))
                       (pw-util:logmsg "Receiving bot response...~%")
                       (loop for line = (read-line stream nil nil)
                             do (write-line line *standard-output*)
                             until (equal line "go")
                             finally (force-output *standard-output*)))
              (ignore-errors (usocket:socket-close socket)))))))))
