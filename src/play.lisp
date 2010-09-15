(in-package :planet-wars)

(defun play (&key (player (make-instance 'dummy-player))
             (input *standard-input*) (output *standard-output*))
  (pw-util:logmsg "~&~%* New game started at ~A~%"
                  (pw-util:current-date-time-string))
  (handler-bind ((error
                  (lambda (e)
                    (pw-util:logmsg
                     "ERROR: ~A~%~A~%"
                     e
                     (with-output-to-string (s)
                       (sb-debug:backtrace most-positive-fixnum s)))
                    (sb-ext:quit :recklessly-p t))))
    (loop while (peek-char nil input nil nil)
          for turn from 1 do
          (pw-util:logmsg "** turn ~A~%" turn)
          (pw-util:logmsg "~A~%"
                          (with-output-to-string (*trace-output*)
                            (time
                             (progn
                               (parse-game player input)
                               (let ((orders (compute-orders player)))
                                 (pw-util:logmsg "*** orders~%~S~%" orders)
                                 (write-orders orders output))
                               (write-line "go" output))))))
    ;; Sometimes necessary because output streams can be closed and
    ;; UNIX-EXIT runs into an error.
    #+nil
    (sb-ext:quit :recklessly-p t)))

(defun start-server-for-proxy-bot (&key (player (make-instance 'dummy-player)))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        client
        stream)
    (socket-bind socket #(127 0 0 1) 41807)
    (socket-listen socket 0)
    (pw-util:logmsg "Waiting for connection...~%")
    (setf client (socket-accept socket)
          stream (socket-make-stream client :input t
                                     :output t :element-type 'character
                                     :buffering :line))
    (pw-util:logmsg "Got connection...~%")
    (play :player player :input stream :output stream)))
