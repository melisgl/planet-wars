(defun main (&key
             (player (make-instance 'iteratively-deepening-distance-player))
             (input *standard-input*) (output *standard-output*))
  (logmsg "~&~%=== New Match: ~A ===~%" (current-date-time-string))
  (handler-bind ((error
                  (lambda (e)
                    (logmsg "ERROR: ~A~%~A~%"
                            e
                            (with-output-to-string (s)
                              (sb-debug:backtrace most-positive-fixnum s)))
                    (sb-ext:quit :recklessly-p t))))
    (loop while (peek-char nil input nil nil)
          for move from 0 do
          (logmsg "~A~%"
                  (with-output-to-string (*trace-output*)
                    (time
                     (progn
                       (logmsg "--- move: ~S ---~%" move)
                       (parse-map input player)
                       (print-map (map-of player))
                       (move (compute-move player) output))))))
    ;; Necessary because output streams can be closed and UNIX-EXIT runs
    ;; into an error.
    (sb-ext:quit :recklessly-p t)))

(defun main ()
  (open-log)
  ;; Comment this out to always get the same random numbers.
  (setf *random-state* (make-random-state t))
  ;; Again, not pretty but we'll avoid compiler warnings.
  (let (#+pwbot-slime socket #+pwbot-slime client #+pwbot-slime stream)
    #+pwbot-slime (progn (setf socket (make-instance 'inet-socket :type :stream
                                                     :protocol :tcp))
                         (socket-bind socket #(127 0 0 1) 41807)
                         (socket-listen socket 0)
                         (logmsg "Waiting for connection...~%")
                         (setf client (socket-accept socket)
                               stream (socket-make-stream client :input t
                                        :output t :element-type 'character
                                        :buffering :line)
                               *input* stream
                               *output* stream))
    #-pwbot-slime (setf *input* *standard-input*
                        *output* *standard-output*)
    (logmsg "=== New Match: " (current-date-time-string) " ===~%")
    (unwind-protect
         (loop while (peek-char nil *input* nil)
               for turn from 1
               for move-start-time = (wall-time)
               for move-end-time = (wall-time :offset +max-turn-time+)
               do (logmsg "--- turn: " turn " ---~%")
                  (logmsg "[start] " (current-date-time-string) "~%")
                  (parse-game-state)  ; sets *fleets* and *planets*
                  (logmsg "Sending orders... ")
                  (bot-think)
                  (finish-turn)
                  (let ((wall-time (wall-time)))
                    (logmsg "[  end] move took " (- wall-time move-start-time)
                            " seconds (" (- move-end-time wall-time)
                            " left).~%")))
      #+pwbot-slime (progn (socket-close client)
                           (socket-close socket))
      (close-log))))
