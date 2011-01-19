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
        (write-line "go" output)
        (force-output output)))

(defun start-server-for-proxy-bot (&key (player-class 'bocsimacko) one-shot)
  (let ((socket (usocket:socket-listen #+allegro "localhost"
                                       #+sbcl #(127 0 0 1)
                                       41807 :reuse-address t)))
    (unwind-protect
         (loop do
               (pw-util:logmsg "Waiting for connection...~%")
               (let* ((client (usocket:socket-accept socket))
                      (stream (usocket:socket-stream client)))
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
                      (ignore-errors (usocket:socket-close client))))))
               until one-shot)
      (ignore-errors (usocket:socket-close socket)))))
