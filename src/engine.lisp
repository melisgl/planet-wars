(in-package :planet-wars)

(defun make-piped-streams ()
  (multiple-value-bind (in out) (sb-posix:pipe)
    (values (sb-sys:make-fd-stream in :input t :output nil :auto-close t)
            (sb-sys:make-fd-stream out :output t :input nil :auto-close t))))

(defun play-match (game player1 player2 &key trace)
  (multiple-value-bind (in-1-r in-1-w) (make-piped-streams)
    (multiple-value-bind (out-1-r out-1-w) (make-piped-streams)
      (multiple-value-bind (in-2-r in-2-w) (make-piped-streams)
        (multiple-value-bind (out-2-r out-2-w) (make-piped-streams)
          (let ((thread-1 (sb-thread:make-thread (lambda ()
                                                   (play player1
                                                         :input in-1-r
                                                         :output out-1-w))
                                                 :name "Player 1"))
                (thread-2 (sb-thread:make-thread (lambda ()
                                                   (play player2
                                                         :trace t
                                                         :input in-2-r
                                                         :output out-2-w))
                                                 :name "Player 2")))
            (when trace
              (write-game game *trace-output*))
            (unwind-protect
                 (loop for winner = (winner game)
                       do
                       (when winner
                         )
                       (write-game game in-1-w)
                       (force-output in-1-w)
                       (reverse-game game)
                       (write-game game in-2-w)
                       (force-output in-2-w)
                       (reverse-game game)
                       (execute-orders game 1 (read-orders out-1-r))
                       (execute-orders game 2 (read-orders out-2-r))
                       
                       (destructuring-bind (old-x1 old-y1) (position-1 map)
                         (destructuring-bind (old-x2 old-y2) (position-2 map)
                           (destructuring-bind (x1 y1)
                               (neighbour move-1 old-x1 old-y1)
                             (destructuring-bind (x2 y2)
                                 (neighbour move-2 old-x2 old-y2)
                               (let ((lost-1 (or (wallp board x1 y1)
                                                 (and (= x1 x2) (= y1 y2))))
                                     (lost-2 (or (wallp board x2 y2)
                                                 (and (= x1 x2) (= y1 y2)))))
                                 (setf (aref board old-y1 old-x1) #\#)
                                 (setf (aref board old-y2 old-x2) #\#)
                                 (setf (aref board y1 x1) #\1)
                                 (setf (aref board y2 x2) #\2)
                                 (setf (slot-value map 'position-1) (list x1 y1))
                                 (setf (slot-value map 'position-2) (list x2 y2))
                                 (when trace
                                   (write-map map *trace-output*))
                                 (when (or lost-1 lost-2)
                                   (return-from play-match
                                     (values lost-1 lost-2)))))))))
              (ignore-errors (sb-thread:terminate-thread thread-1))
              (ignore-errors (sb-thread:terminate-thread thread-2))
              (sb-thread:join-thread thread-1 :default nil)
              (sb-thread:join-thread thread-2 :default nil))))))))

#|

(defun self-play (map p1 p2 &key (trace t))
  (play-match map p1 p2 :trace trace))

(defparameter *p1* (make-instance 'distance-player :max-depth 2))
(defparameter *p2* (make-instance 'distance-player :max-depth 2))
(defparameter *p1* (make-instance 'iteratively-deepening-distance-player))
(defparameter *p2* (make-instance 'iteratively-deepening-distance-player))

(defun self-play-2 (name p1 p2)
  (values (multiple-value-list
           (self-play (load-map name) p1 p2))
          (multiple-value-list
           (self-play (load-map name) p2 p1))))

(time (self-play (load-map "maps/ring.txt") *p1* *p2*))
(time (self-play (load-map "maps/joust.txt") *p1* *p2*))
(time (self-play (load-map "maps/keyhole.txt") *p1* *p2*))
(time (self-play (load-map "maps/quadrant.txt") *p1* *p2*))
(time (self-play (load-map "maps/toronto.txt") *p1* *p2*))
(time (self-play (load-map "maps/trix.txt") *p1* *p2*))
(time (self-play (load-map "maps/duel-small.txt") *p1* *p2*))
(time (self-play (load-map "maps/box.txt") *p1* *p2*))
(time (self-play (load-map "maps/gate.txt") *p1* *p2*))
(time (self-play (load-map "maps/oval.txt") *p1* *p2*))
(time (self-play (load-map "maps/plus.txt") *p1* *p2*))
(time (self-play (load-map "maps/toronto2.txt") *p1* *p2*))
(time (self-play (load-map "maps/vee.txt") *p1* *p2*))
(time (self-play (load-map "maps/triangle.txt") *p1* *p2*))
(time (self-play (load-map "maps/15x16.txt") *p1* *p2*))

(self-play-2 "maps/u.txt" *p1* *p2*)
(self-play-2 "maps/trix.txt" *p1* *p2*)
(self-play-2 "maps/quadrant.txt" *p1* *p2*)
(self-play-2 "maps/toronto.txt" *p1* *p2*)
(self-play-2 "maps/apocalyptic.txt" *p1* *p2*)
(self-play-2 "maps/joust.txt" *p1* *p2*)
(self-play-2 "maps/keyhole.txt" *p1* *p2*)
(self-play-2 "maps/ring.txt" *p1* *p2*)

(with-input-from-string (s *map-mistake-3*)
  (self-play (parse-map s (make-instance 'tron-map))))

(require :sb-sprof)

(progn
  (sb-sprof:reset)
  (sb-sprof:start-profiling)
  (sleep 10)
  (sb-sprof:stop-profiling)
  (sb-sprof:report :type :graph))

(time (self-play (load-map "maps/keyhole.txt")))

|#
