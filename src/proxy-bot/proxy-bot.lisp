(in-package :pw-proxy-bot)

(defun connection-refused (&optional arg)
  (declare (ignore arg))
  (logmsg "Connection refused. Aborting...~%")
  (cl-user::quit :unix-status 111))

(defun proxy ()
  (setq pw-util:*verbose* t)
  (setq pw-util:*log-filename* "proxy-bot.log")
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (pw-util:logmsg "* ProxyBot started at ~A~%" (current-date-time-string))
    (logmsg "Connecting to real bot at 127.0.0.1:41807...~%")
    (handler-bind ((connection-refused-error #'connection-refused))
      (socket-connect socket #(127 0 0 1) 41807))
    (unwind-protect
         (loop with stream = (socket-make-stream socket :input t :output t
                                                 :element-type 'character
                                                 :buffering :line)
               while (peek-char nil *standard-input* nil)
               for turn from 1
               do (logmsg "** turn: ~S~%" turn)
               (logmsg "Sending game state...~%")
               (loop for line = (read-line *standard-input* nil)
                     do (write-line line stream)
                     until (equal line "go")
                     finally (force-output stream))
               (logmsg "Receiving bot response...~%")
               (loop for line = (read-line stream nil nil)
                     do (write-line line *standard-output*)
                     until (equal line "go")
                     finally (force-output *standard-output*)))
      (socket-close socket))))
