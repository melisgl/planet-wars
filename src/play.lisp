(in-package :planet-wars)

;;; Called by the MyBot binary.
(defun main ()
  (play))

(defun play (&key (player (make-instance 'dummy-player))
             (input *standard-input*) (output *standard-output*)
             (exit-on-error-p t))
  (pw-util:logmsg "~&~%* New game started at ~A~%"
                  (pw-util:current-date-time-string))
  (handler-bind ((error
                  (lambda (e)
                    (pw-util:logmsg
                     "ERROR: ~A~%~A~%"
                     e
                     (with-output-to-string (s)
                       (sb-debug:backtrace most-positive-fixnum s)))
                    (when exit-on-error-p
                      (sb-ext:quit :recklessly-p t)))))
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
                               (write-line "go" output))))))))

(defun start-server-for-proxy-bot (&key (player (make-instance 'dummy-player))
                                   one-shot)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (setf (sockopt-reuse-address socket) t)
           (socket-bind socket #(127 0 0 1) 41807)
           (socket-listen socket 0)
           (loop do
                 (pw-util:logmsg "Waiting for connection...~%")
                 (let* ((client (socket-accept socket))
                        (stream (socket-make-stream client :input t
                                                    :output t
                                                    :element-type 'character
                                                    :buffering :line)))
                   (pw-util:logmsg "Got connection...~%")
                   (#+sb-thread sb-thread:make-thread #-sb-thread funcall
                                (lambda ()
                                  (unwind-protect
                                       (play :player player
                                             :input stream :output stream
                                             :exit-on-error-p nil)
                                    (socket-close client)))))
                 until one-shot))
      (socket-close socket))))
