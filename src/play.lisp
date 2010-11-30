(in-package :planet-wars)

;;; Called by the MyBot binary.
(defun main ()
  (pw-util:with-reckless-exit
    (pw-util:with-errors-logged (:exit-on-error-p t)
      (play))))


(defgeneric compute-orders (bot input))

(defun play (&key (player (make-instance 'bocsimacko))
             (input *standard-input*) (output *standard-output*))
  (pw-util:logmsg "~&~%* game started at ~A~%"
                  (pw-util:current-date-time-string))
  (loop while (peek-char nil input nil nil)
        for turn from 1 do
        (pw-util:logmsg "** turn ~A~%" turn)
        (let* ((orders (compute-orders player input))
               (orders-now (remove-if-not #'current-order-p orders))
               (orders-later (remove-if #'current-order-p orders)))
          (pw-util:logmsg "*** orders~%~S~%" orders-now)
          (when orders-later
            (pw-util:logmsg "*** orders later~%~S~%" orders-later))
          (write-orders orders-now output))
        (write-line "go" output)))

(defun start-server-for-proxy-bot (&key (player-class 'dummy-player) one-shot)
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
                   (#+sb-thread
                    sb-thread:make-thread
                    #-sb-thread
                    funcall
                    (lambda ()
                      (unwind-protect
                           (pw-util:with-errors-logged ()
                             (play :player (make-instance player-class)
                                   :input stream :output stream))
                        (ignore-errors (socket-close client))))))
                 until one-shot))
      (ignore-errors (socket-close socket)))))
