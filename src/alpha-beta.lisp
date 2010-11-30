(in-package :planet-wars)

(defun alpha-beta (state depth &key alpha beta
                   maybe-evaluate-state list-actions call-with-action trace
                   (frame #'funcall) new-best)
  (labels ((foo (state depth alpha beta)
             (funcall frame
                      (lambda (state depth alpha beta)
                        (multiple-value-bind (score actions)
                            (multiple-value-bind (evaluation-score evaluation-actions)
                                (funcall maybe-evaluate-state state depth)
                              (cond (evaluation-score
                                     (values evaluation-score evaluation-actions))
                                    (t
                                     (let ((max nil)
                                           (max-actions nil)
                                           (actions (funcall list-actions state depth)))
                                       (unless actions
                                         (error "No actions for non terminal state ~S at depth ~S"
                                                state depth))
                                       (dolist (action actions
                                                (values max max-actions))
                                         (multiple-value-bind (score actions)
                                             (funcall call-with-action
                                                      state depth action
                                                      (lambda (state)
                                                        (foo state (1+ depth)
                                                             (if beta (- beta) nil)
                                                             (if alpha (- alpha) nil))))
                                           (when (or (null alpha) (< alpha (- score)))
                                             (setq alpha (- score)))
                                           (when (or (null max) (< max (- score)))
                                             (setq max (- score))
                                             (setq max-actions (cons action actions))
                                             (when new-best
                                               (funcall new-best depth max max-actions)))
                                           (when (and alpha beta (<= beta alpha))
                                             (return (values alpha :alpha-beta-cut)))))))))
                          (when trace
                            (funcall trace state depth score actions))
                          (values score actions)))
                      state depth alpha beta)))
    (foo state depth alpha beta)))

(defun player-to-move (depth)
  (if (zerop (mod depth 2))
      1
      2))

(defparameter *widths* nil)
(defvar *deferred-orders* nil)

(defun split-past-and-future-orders (turn orders)
  (values (remove-if-not (lambda (order)
                           (<= (turn order) turn))
                         orders)
          (remove-if (lambda (order)
                       (<= (turn order) turn))
                     orders)))

(defmacro with-undeferred-orders ((turn) &body body)
  (alexandria:with-unique-names (past-orders future-orders)
    `(multiple-value-bind (,past-orders ,future-orders)
         (split-past-and-future-orders ,turn *deferred-orders*)
       (let ((*deferred-orders* ,future-orders))
         (with-orders (,past-orders)
           ,@body)))))

(defun maybe-evaluate-state (game depth)
  (if (= depth (length *widths*))
      (progn
        (values
         (* (if (= 0 (mod depth 2)) 1 -1)
            (handler-bind ((future-impossible #'continue))
              (with-undeferred-orders (most-positive-fixnum)
                (evaluate/full-attack (planets game)
                                      1
                                      :min-turn-to-depart-1 2
                                      :min-turn-to-depart-2 1
                                      :positional-min-turn-to-depart-1 3))))
         nil))
      nil))

(defun subseq* (seq start end)
  (subseq seq start (min end (length seq))))

(defun list-actions (game depth)
  (if (= 0 (aref *widths* depth))
      (list ())
      (handler-bind ((future-impossible #'continue))
        (map 'list #'first
             (subseq*
              (generate-and-score-moves game (player-to-move depth))
              0 (aref *widths* depth))))))

(defun call-with-action (game depth orders fn)
  (when *verbose*
    (loop repeat (1+ depth) do (logmsg "*"))

    (logmsg " playing ~S~%" orders))
  (with-orders (orders)
    (let* ((*depth* (1+ depth))
           ;; This means that the opponent (at depth 1) will see the
           ;; moves at depth 0 and react still on turn 0.
           (*turn* (floor (1+ depth) 2)))
      (funcall fn game))))

(defun trace-alpha-beta (game depth score moves)
  (declare (ignore game))
  (when *verbose*
    (loop repeat (1+ depth) do (logmsg "*"))
    (let ((move (if (listp moves)
                    (first moves)
                    moves)))
      (logmsg " SCORE=~S BEST-MOVE=~S~%"
              (float score 0.0)
              move))))

(defun alpha-beta* (game &key widths)
  (let* ((*widths* (subseq* (coerce widths 'vector)
                            0 (min (length widths)
                                   (* 2 *n-turns-till-horizon*))))
         (finished-one-alpha-beta-first-move-p nil))
    (alpha-beta game 0 :maybe-evaluate-state #'maybe-evaluate-state
                :list-actions #'list-actions
                :call-with-action #'call-with-action
                :trace #'trace-alpha-beta
                :new-best (lambda (depth score actions)
                            (when (= 0 depth)
                              (sb-sys:without-interrupts
                                ;; Before one depth 0 move is scored
                                ;; we want to return the best move
                                ;; found by the move generator.
                                (unless finished-one-alpha-beta-first-move-p
                                  (setq *best-move-so-far* nil)
                                  (setq finished-one-alpha-beta-first-move-p t))
                                (register-evaluated-move
                                 (first actions)
                                 score)))))))
