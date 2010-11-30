(in-package :planet-wars)

(defvar *depth*)
(defvar *turn*)
(declaim (type (unsigned-byte 16) *depth* *turn*))


;;;; Keep track of the best ply 0 move found so far so that at a
;;;; timeout we can return something sane.

(defvar *best-move-so-far*)

(defun register-evaluated-move (move score)
  (when (and (= 0 *depth*)
             (or (null *best-move-so-far*)
                 (< (cdr *best-move-so-far*) score)))
    (setq *best-move-so-far* (cons move score))))

(defmacro with-best-move-on-timeout (timeout &body body)
  `(let ((*best-move-so-far* nil))
     (handler-case
         (sb-ext:with-timeout ,timeout
           ,@body)
       (sb-ext:timeout ()
         (logmsg "*** timed out~%")
         (if (null *best-move-so-far*)
             ()
             (car *best-move-so-far*))))))


;;;; Future

;;; A future is a particular sequence of states of a planet. It's
;;; represented by an OWNERS and a N-SHIPS array.
(defclass future ()
  ((planet :initarg :planet :reader planet)
   (owners :initarg :owners :reader owners)
   (n-ships :initarg :n-ships :reader n-ships)
   ;; Number of ships player 2 lost when attacking neutrals minus the
   ;; number of ships player 1 lost when attacking neutrals in this
   ;; future.
   (balance :initarg :balance :reader balance)))

;;; Print futures like this: #<FUTURE :ID 0 :OWNER 0->1 :BALANCE -7>
;;; where ID is the id of the planet, 0->1 means that it starts out
;;; being neutral and is owned by player 1 at the end of the future.
(defmethod print-object ((future future) stream)
  (pprint-logical-block (stream ())
    (print-unreadable-object (future stream :type t)
      (format stream "~S ~S ~S ~S->~S ~S ~S"
              :id (ignore-errors (id (planet future)))
              :owner
              (ignore-errors (first-owner future))
              (ignore-errors (last-owner future))
              :balance
              (ignore-errors (balance future)))))
  future)

(defun first-owner (future)
  (elt (owners future) *turn*))

(defun last-owner (future)
  (alexandria:last-elt (owners future)))

;;; This condition is signalled by COMPUTE-FUTURE* when a departure
;;; scheduled for some future turn is impossible.
(define-condition future-impossible (error)
  ())

;;; Return two vectors (owner and ship count) indexed by turns
;;; representing the future and the balance as the third value. At
;;; turn 0 the owner is INITIAL-OWNER and the ship count is
;;; INITIAL-N-SHIPS. ARRIVALS-1 is a vector indexed by turn whose
;;; values are the number of ships of player 1 arriving to PLANET.
;;; DEPARTURES-1, ARRIVALS-1, DEPARTURES-2 are similar.
;;;
;;; RESOLVE-BATTLE-FN is a function of five parameters: the current
;;; owner, the number of neutral ships, player 1 ships and player 2
;;; ships and the turn on which the battle is taking place.
;;;
;;; If DEPARTURES-1 is non-zero on some turn and the planet is not
;;; owned by player 1 or there aren't enough ships FUTURE-IMPOSSIBLE
;;; is signalled (similarly for DEPARTURES-2). The caller may CONTINUE
;;; from this error.
(defun compute-future* (planet &key (initial-owner (owner planet))
                        (initial-n-ships (n-ships planet))
                        (arrivals-1 (arrivals-1 planet))
                        (arrivals-2 (arrivals-2 planet))
                        (departures-1 (departures-1 planet))
                        (departures-2 (departures-2 planet))
                        (resolve-battle-fn #'resolve-battle))
  (declare (type ship-count-vector arrivals-1 arrivals-2
                 departures-1 departures-2)
           (type function resolve-battle-fn))
  (let* ((n-turns (length arrivals-1))
         (owners (make-player-vector n-turns))
         (n-shipss (make-count-vector n-turns))
         (owner initial-owner)
         (n-ships initial-n-ships)
         (balance 0)
         (growth (growth planet)))
    (declare (type ship-count n-ships balance growth))
    (dotimes (turn n-turns)
      (declare (optimize speed))
      ;; arrival phase
      (let ((arrival-1 (aref arrivals-1 turn))
            (arrival-2 (aref arrivals-2 turn)))
        (ecase owner
          ((0)
           (when (or (/= 0 arrival-1) (/= 0 arrival-2))
             (decf balance (min n-ships arrival-1))
             (incf balance (min n-ships arrival-2))
             (setf (values owner n-ships)
                   (funcall resolve-battle-fn owner
                            n-ships arrival-1 arrival-2 turn))))
          ((1)
           (incf n-ships arrival-1)
           (when (/= 0 arrival-2)
             (setf (values owner n-ships)
                   (funcall resolve-battle-fn owner 0 n-ships arrival-2
                            turn))))
          ((2)
           (incf n-ships arrival-2)
           (when (/= 0 arrival-1)
             (setf (values owner n-ships)
                   (funcall resolve-battle-fn owner 0 arrival-1 n-ships
                            turn))))))
      ;; set the owner and the ship count
      (setf (aref owners turn) owner)
      (setf (aref n-shipss turn) n-ships)
      ;; departure phase
      (let ((departure-1 (aref departures-1 turn))
            (departure-2 (aref departures-2 turn)))
        (ecase owner
          ((0)
           (unless (and (= 0 departure-1) (= 0 departure-2))
             (cerror "Continue" 'future-impossible)))
          ((1)
           (decf n-ships departure-1)
           (unless (<= 0 n-ships)
             (cerror "Continue" 'future-impossible)
             (setq n-ships 0))
           (unless (= 0 departure-2)
             (cerror "Continue" 'future-impossible))
           (incf n-ships growth))
          ((2)
           (decf n-ships departure-2)
           (unless (<= 0 n-ships)
             (cerror "Continue" 'future-impossible)
             (setq n-ships 0))
           (unless (= 0 departure-1)
             (cerror "Continue" 'future-impossible))
           (incf n-ships growth)))))
    (values owners n-shipss balance)))

(defun compute-future (planet)
  (multiple-value-bind (owners n-ships balance)
      (compute-future* planet)
    (make-instance 'future :planet planet :owners owners :n-ships n-ships
                   :balance balance)))


;;;; Surplus

;;; The cumulative surplus of player P at planet A at time t is the
;;; number of ships that can be sent away on that turn from the
;;; defending army without causing the planet to be lost anytime after
;;; that (observing only the fleets already in space and assuming that
;;; no previous surplus ships were previously sent away ) or bringing
;;; an imminent loss closer in time (if INCLUDE-ATTACKED-P). Else the
;;; surplus is 0. 
;;;
;;; At any turn at most one of the players can have a non-zero surplus
;;; hence the surplus vectors of the two players can be stuffed into a
;;; single vector by making surpluses of player 2 negative. This
;;; function returns this combined surplus vector.
;;;
;;; Surpluses of player 1 before MIN-TURN-1 and surpluses of player 2
;;; before MIN-TURN-2 are 0.
(defun cumulative-surplus (future &key (min-turn-1 *turn*) (min-turn-2 *turn*)
                           include-attacked-p)
  (declare (type fixnum min-turn-1 min-turn-2)
           (optimize speed))
  (let* ((owners (owners future))
         (last-owner (last-owner future))
         (n-ships (n-ships future))
         (current-owner -1)
         (current-surplus-1 (1- (expt 2 15)))
         (current-surplus-2 (1- (expt 2 15)))
         (departures-1 (departures-1 (planet future)))
         (departures-2 (departures-2 (planet future)))
         (n-turns (length owners))
         (surplus (make-count-vector n-turns)))
    (declare (type player last-owner)
             (type player-vector owners)
             (type ship-count-vector n-ships departures-1 departures-2)
             (type ship-count current-surplus-1 current-surplus-2))
    (loop for i downfrom (- n-turns 1) downto *turn*
          for owner = (aref owners i)
          while (and (/= 0 owner)
                     (or include-attacked-p
                         (= owner last-owner)))
          do
          (let ((effective-n-ships (- (aref n-ships i)
                                      (aref (if (= owner 1)
                                                departures-1
                                                departures-2)
                                            i))))
            (when (minusp effective-n-ships)
              (cerror "Continue" 'future-impossible)
              (setq effective-n-ships 0))
            (if (= 1 owner)
                (setf current-surplus-1
                      (min (if (= 2 current-owner)
                               (max 0 (1- current-surplus-1))
                               current-surplus-1)
                           effective-n-ships))
                (setf current-surplus-2
                      (min (if (= 1 current-owner)
                               (max 0 (1- current-surplus-2))
                               current-surplus-2)
                           effective-n-ships)))
            (setf current-owner owner))
          (when (and (= owner 1) (<= min-turn-1 i))
            (setf (aref surplus i) current-surplus-1))
          (when (and (= owner 2) (<= min-turn-2 i))
            (setf (aref surplus i) (- current-surplus-2))))
    surplus))

(defun uncumulate-surplus! (surplus)
  (declare (type ship-count-vector surplus)
           (optimize speed))
  (let ((sum 0))
    (declare (type ship-count sum))
    (loop for j upfrom 0 below (length surplus) do
          (let ((s (aref surplus j)))
            (when (or (and (minusp sum) (plusp s))
                      (and (plusp sum) (minusp s)))
              (setq sum 0))
            (decf (aref surplus j) sum)
            (incf sum (aref surplus j))))
    surplus))

(defun uncumulate-surplus (surplus)
  (let ((x (make-array (length surplus) :element-type 'ship-count)))
    (replace x surplus)
    (uncumulate-surplus! x)))

;;; The non-cumulative version: surplus at turn t is in addition to
;;; surplus at the previous turn. In other words, assume that all
;;; previous surpluses were sent away.
(defun surplus (future &key (min-turn-1 *turn*) (min-turn-2 *turn*)
                include-attacked-p)
  (uncumulate-surplus!
   (cumulative-surplus future :min-turn-1 min-turn-1
                       :min-turn-2 min-turn-2
                       :include-attacked-p include-attacked-p)))


;;;; Full attack future

(defclass full-attack-future (future) ())

;;; Compute the full attack future for PLANET in which all planets
;;; send their SURPLUSES to it continuously. SURPLUSES is a vector of
;;; surplus vectors indexed by planet ids.
;;;
;;; MIN-TURN-TO-DEPART-1 is the earliest turn for any planet of player
;;; 1 to start sending ships. MIN-TURN-TO-ARRIVE-1 is the earliest
;;; turn we want ships to arrive: useful when one wants to arrive to a
;;; neutral after the opponent.
;;; 
;;; If BALANCE is given then it overrides the computed balance.
;;;
;;; Behind the scenes some intermediate results are cached.
;;; SURPLUSES-CACHE-KEY identifies the SURPLUSES for this purpose. If
;;; it's NIL then no caching takes place, else it becomes a part of
;;; the key in an EQUAL hash table.
(defun compute-full-attack-future (planet surpluses &key
                                   surpluses-cache-key
                                   (min-turn-to-arrive-1 *turn*)
                                   (min-turn-to-arrive-2 *turn*)
                                   (min-turn-to-depart-1 *turn*)
                                   (min-turn-to-depart-2 *turn*)
                                   balance)
  (multiple-value-bind (arrivals-1 arrivals-2)
      (make-full-attack-arrivals planet surpluses
                                 :surpluses-cache-key surpluses-cache-key
                                 :min-turn-to-arrive-1 min-turn-to-arrive-1
                                 :min-turn-to-arrive-2 min-turn-to-arrive-2
                                 :min-turn-to-depart-1 min-turn-to-depart-1
                                 :min-turn-to-depart-2 min-turn-to-depart-2)
    (multiple-value-bind (owners n-ships full-attack-future-balance)
        (compute-future* planet :arrivals-1 arrivals-1 :arrivals-2 arrivals-2)
      (make-instance 'full-attack-future
                     :planet planet :owners owners :n-ships n-ships
                     :balance (or balance full-attack-future-balance)))))

;;; A neighbour is TURNS-TO-TRAVEL away from a planet. Assume that the
;;; neighbour's SURPLUS is sent every turn to the planet. Add the
;;; arrivals from SURPLUSES to ARRIVALS-1 and ARRIVALS-2.
;;;
;;; That was the basic scenario, one can also put constraints on
;;; arrival and departure time. If player 2 takes the neutral on turn
;;; X then by passing (1+ X) as MIN-TURN-TO-ARRIVE-1 one can make sure
;;; that player 1's ships will arrive after that takeover. One can
;;; also use MIN-TURN-TO-DEPART-1 to simulate reactions: if player 2
;;; departs on turn Y with the fleet that will take over the neutral
;;; then (1+ Y) as MIN-TURN-TO-DEPART-1 is a sensible value.
;;;
;;; If SUBTRACTP then don't add to arrivals but subtract from them
;;; which is useful for updating previously calculated arrival
;;; vectors.
(defun add-surplus-into-arrivals (surplus arrivals-1 arrivals-2 turns-to-travel
                                  &key subtractp cumulativep
                                  (min-turn-to-arrive-1 *turn*)
                                  (min-turn-to-arrive-2 *turn*)
                                  (min-turn-to-depart-1 *turn*)
                                  (min-turn-to-depart-2 *turn*))
  (declare (type ship-count-vector surplus arrivals-1 arrivals-2)
           (type fixnum turns-to-travel
                 min-turn-to-arrive-1 min-turn-to-arrive-2
                 min-turn-to-depart-1 min-turn-to-depart-2)
           (optimize speed))
  (let ((min-turn-to-arrive-1 (max turns-to-travel
                                   min-turn-to-arrive-1
                                   (+ min-turn-to-depart-1 turns-to-travel)))
        (min-turn-to-arrive-2 (max turns-to-travel
                                   min-turn-to-arrive-2
                                   (+ min-turn-to-depart-2 turns-to-travel)))
        (n (length arrivals-1)))
    (when (or (< min-turn-to-arrive-1 n)
              (< min-turn-to-arrive-2 n))
      (let ((sum-1 0)
            (sum-2 0))
        (declare (type ship-count sum-1 sum-2))
        (loop for i upfrom turns-to-travel below n
              do (let ((s (aref surplus (- i turns-to-travel))))
                   (declare (type ship-count s))
                   (when (= i min-turn-to-arrive-1)
                     (unless cumulativep
                       (incf s sum-1)))
                   (when (= i min-turn-to-arrive-2)
                     (unless cumulativep
                       (incf s sum-2)))
                   (cond ((= 0 s))
                         ((plusp s)
                          (cond ((< i min-turn-to-arrive-1)
                                 (incf sum-1 s))
                                ((>= i min-turn-to-arrive-1)
                                 (if subtractp
                                     (decf (aref arrivals-1 i) s)
                                     (incf (aref arrivals-1 i) s)))))
                         (t
                          (cond ((< i min-turn-to-arrive-2)
                                 (incf sum-2 s))
                                ((>= i min-turn-to-arrive-2)
                                 (if subtractp
                                     (incf (aref arrivals-2 i) s)
                                     (decf (aref arrivals-2 i) s))))))))))))

;;; The rest of stuff on this page (until ^L) is just for performance:
;;; caching of full attack arrivals and updating the arrivals in light
;;; of some moves made since the time of caching.

;;; The cache unaware version.
(defun compute-full-attack-arrivals (planet surpluses &key
                                     (add-normal-arrivals-p t)
                                     cumulativep
                                     (min-turn-to-arrive-1 *turn*)
                                     (min-turn-to-arrive-2 *turn*)
                                     (min-turn-to-depart-1 *turn*)
                                     (min-turn-to-depart-2 *turn*))
  (let* ((n (length (arrivals-2 planet)))
         (arrivals-1 (make-count-vector n))
         (arrivals-2 (make-count-vector n)))
    (declare (type ship-count-vector arrivals-1 arrivals-2))
    (when add-normal-arrivals-p
      (locally (declare (optimize speed))
        (replace arrivals-1 (the ship-count-vector (arrivals-1 planet)))
        (replace arrivals-2 (the ship-count-vector (arrivals-2 planet)))))
    ;; add the surpluses to the arrival vectors
    (do-neighbours ((turn neighbours) planet)
      (dolist (neighbour neighbours)
        (alexandria:when-let (surplus (aref surpluses (id neighbour)))
          (add-surplus-into-arrivals
           surplus
           arrivals-1 arrivals-2 turn
           :cumulativep cumulativep
           :min-turn-to-arrive-1 min-turn-to-arrive-1
           :min-turn-to-arrive-2 min-turn-to-arrive-2
           :min-turn-to-depart-1 min-turn-to-depart-1
           :min-turn-to-depart-2 min-turn-to-depart-2))))
    (values arrivals-1 arrivals-2)))

(defun update-full-attack-arrivals (planet old-arrivals-1 old-arrivals-2
                                    old-surpluses orders-since surpluses
                                    &key (min-turn-to-arrive-1 *turn*)
                                    (min-turn-to-arrive-2 *turn*)
                                    (min-turn-to-depart-1 *turn*)
                                    (min-turn-to-depart-2 *turn*))
  (let* ((n (length old-arrivals-1))
         (arrivals-1 (make-count-vector n))
         (arrivals-2 (make-count-vector n)))
    (declare (type ship-count-vector arrivals-1 arrivals-2))
    (locally (declare (optimize speed))
      (replace arrivals-1 (the ship-count-vector old-arrivals-1))
      (replace arrivals-2 (the ship-count-vector old-arrivals-2)))
    (let ((planets (planets-involved-in-move orders-since)))
      (dolist (neighbour planets)
        (if (eq neighbour planet)
            (dolist (order orders-since)
              (when (eq planet (destination order))
                (incf (aref (if (= 1 (owner order))
                                arrivals-1
                                arrivals-2)
                            (arrival-turn order))
                      (n-ships order))))
            (let ((id (id neighbour))
                  (turn (turns-to-travel planet neighbour)))
              (alexandria:when-let (old-surplus (aref old-surpluses id))
                (add-surplus-into-arrivals
                 old-surplus
                 arrivals-1 arrivals-2 turn
                 :subtractp t
                 :min-turn-to-arrive-1 min-turn-to-arrive-1
                 :min-turn-to-arrive-2 min-turn-to-arrive-2
                 :min-turn-to-depart-1 min-turn-to-depart-1
                 :min-turn-to-depart-2 min-turn-to-depart-2))
              (alexandria:when-let (surplus (aref surpluses id))
                (add-surplus-into-arrivals
                 surplus
                 arrivals-1 arrivals-2 turn
                 :min-turn-to-arrive-1 min-turn-to-arrive-1
                 :min-turn-to-arrive-2 min-turn-to-arrive-2
                 :min-turn-to-depart-1 min-turn-to-depart-1
                 :min-turn-to-depart-2 min-turn-to-depart-2))))))
    (values arrivals-1 arrivals-2)))

;;; Find it in the cache or compute new arrival vectors.
(defun make-full-attack-arrivals (planet surpluses &key
                                  surpluses-cache-key
                                  (min-turn-to-arrive-1 *turn*)
                                  (min-turn-to-arrive-2 *turn*)
                                  (min-turn-to-depart-1 *turn*)
                                  (min-turn-to-depart-2 *turn*))
  (let ((key (list surpluses-cache-key
                   min-turn-to-arrive-1
                   min-turn-to-arrive-2
                   min-turn-to-depart-1
                   min-turn-to-depart-2)))
    (multiple-value-bind (values foundp orders-since)
        (if surpluses-cache-key
            (lookup-cached-stuff planet key)
            nil)
      (let ((planets-involved (planets-involved-in-move orders-since)))
        (if (and foundp
                 (< (length planets-involved) (/ (length surpluses) 2)))
            (destructuring-bind (old-arrivals-1 old-arrivals-2 old-surpluses)
                values
              (update-full-attack-arrivals
               planet old-arrivals-1 old-arrivals-2 old-surpluses
               orders-since surpluses
               :min-turn-to-arrive-1 min-turn-to-arrive-1
               :min-turn-to-arrive-2 min-turn-to-arrive-2
               :min-turn-to-depart-1 min-turn-to-depart-1
               :min-turn-to-depart-2 min-turn-to-depart-2))
            (multiple-value-bind (arrivals-1 arrivals-2)
                (compute-full-attack-arrivals
                 planet surpluses
                 :min-turn-to-arrive-1 min-turn-to-arrive-1
                 :min-turn-to-arrive-2 min-turn-to-arrive-2
                 :min-turn-to-depart-1 min-turn-to-depart-1
                 :min-turn-to-depart-2 min-turn-to-depart-2)
              (when surpluses-cache-key
                (set-cached-stuff planet key
                                  (list arrivals-1 arrivals-2 surpluses)))
              (values arrivals-1 arrivals-2)))))))


;;;; Evaluation

;;; The score of a future (of a planet) is simply the difference of
;;; growths captured by the players adjusted by the balance of the
;;; future (that is, taking into account the ships lost when capturing
;;; neutrals).
;;;
;;; Give a very slight positional penalty every turn for every enemy
;;; ship. When FUTURE is a FULL-ATTACK-FUTURE then this has the effect
;;; of preferring positions where the friendly ships are near the
;;; enemy.
(defun score (future player)
  (declare (type player player))
  (let ((owners (owners future))
        (n-shipss (n-ships future))
        (growth (growth (planet future)))
        (score (* (player-multiplier player)
                  (float (balance future) 0d0)))
        (opponent (opponent player)))
    (declare (type player-vector owners)
             (type ship-count-vector n-shipss)
             (type ship-count growth)
             (type double-float score)
             (optimize speed))
    (loop for i upfrom 0 below (min (length owners)
                                    ;; the last turn of the game does
                                    ;; not produce growth
                                    (the fixnum *n-turns-left-in-game*))
          do
          (let ((owner (aref owners i))
                (n-ships (aref n-shipss i)))
            (cond ((= owner player)
                   (incf score growth))
                  ((= owner opponent)
                   (decf score growth)
                   (when (= player 1)
                     (decf score (* 0.000000000001d0
                                    ;; NOTE: It's not clear weighting
                                    ;; by the number of remaining
                                    ;; turns is advantageous.
                                    (- (the fixnum *n-turns-till-horizon*) i)
                                    n-ships)))))))
    score))

;;; The total score for all planets is the sum of per planet scores
;;; where each planet is evaluated in a full attack future.
;;;
;;; Planets that are neutral at the horizon only contribute to the
;;; positional score (a small adjustment to the integer ship count
;;; based score). This has the upside of urging the bot to capture
;;; neutrals and downside of being unaware of allowing the opponent to
;;; take a planet unless the search is deeper than one ply.
(defun evaluate/full-attack (planets player &key (min-turn-to-depart-1 *turn*)
                             (min-turn-to-depart-2 *turn*)
                             (positional-min-turn-to-depart-1 *turn*))
  (let* ((normal-futures (map 'vector #'compute-future planets))
         (surpluses (map 'vector (lambda (future)
                                   (if (= 0 (last-owner future))
                                       ;; Don't compute the surplus if
                                       ;; it's all zero.
                                       nil
                                       (surplus
                                        future
                                        :include-attacked-p (plusp *depth*))))
                         normal-futures)))
    (declare (type simple-vector planets normal-futures surpluses))
    ;; Future orders are not always executable, so just CONTINUE.
    (handler-bind ((future-impossible #'continue))
      (loop for planet across planets
            for future across normal-futures
            sum (evaluate-planet player future surpluses
                                 min-turn-to-depart-1
                                 min-turn-to-depart-2
                                 positional-min-turn-to-depart-1)))))

(defun evaluate-planet (player future surpluses
                        min-turn-to-depart-1
                        min-turn-to-depart-2
                        positional-min-turn-to-depart-1)
  (if (= 0 (last-owner future))
      (+ (balance future)
         (* 0.000000000001d0
            (score (compute-full-attack-future
                    (planet future) surpluses
                    :surpluses-cache-key 'evaluate/full-attack
                    :min-turn-to-depart-1 positional-min-turn-to-depart-1
                    :min-turn-to-depart-2 min-turn-to-depart-2
                    ;; KLUDGE: :BALANCE 0 would be more
                    ;; correct as it does not penalize
                    ;; early arrivals and is stronger
                    ;; locally. However, it's much
                    ;; worse on tcp.
                    :balance nil)
                   player)))
      (evaluate-non-neutral-planet player future surpluses
                                   min-turn-to-depart-1
                                   min-turn-to-depart-2
                                   positional-min-turn-to-depart-1)))

(defun first-non-neutral-turn (future)
  (position 0 (owners future) :test-not #'eql :start *turn*))

;;; Return a list of turns worth considering as MIN-TURN-TO-ARRIVE.
(defun candidate-min-turns-to-arrive (future)
  (let ((planet (planet future)))
    (if (and (= 0 (owner planet))
             (< (growth planet) (n-ships planet)))
        ;; In addition to as early as possible (0) try arriving on the
        ;; same turn when the neutral is first taken or the next for a
        ;; good sniping.
        (let ((first-non-neutral-turn (first-non-neutral-turn future)))
          (remove-duplicates (list (1+ first-non-neutral-turn)
                                   0
                                   first-non-neutral-turn)))
        ;; As early as possible.
        (list 0))))

;;; Evaluate FUTURE whose last owner must be non-neutral.
(defun evaluate-non-neutral-planet (player future surpluses
                                    min-turn-to-depart-1
                                    min-turn-to-depart-2
                                    positional-min-turn-to-depart-1)
  (let* ((planet (planet future))
         (turns-to-arrive (candidate-min-turns-to-arrive future)))
    (loop for min-turn-to-arrive in turns-to-arrive
          minimize
          (let ((full-attack-future
                 (compute-full-attack-future
                  planet surpluses
                  :surpluses-cache-key 'evaluate/full-attack
                  :min-turn-to-arrive-1 min-turn-to-arrive
                  :min-turn-to-arrive-2 min-turn-to-arrive
                  :min-turn-to-depart-1 min-turn-to-depart-1
                  :min-turn-to-depart-2 min-turn-to-depart-2
                  :balance (balance future)))
                (positional-future
                 (compute-full-attack-future
                  planet surpluses
                  :surpluses-cache-key 'evaluate/full-attack
                  :min-turn-to-arrive-1 min-turn-to-arrive
                  :min-turn-to-arrive-2 min-turn-to-arrive
                  :min-turn-to-depart-1 positional-min-turn-to-depart-1
                  :min-turn-to-depart-2 min-turn-to-depart-2
                  :balance nil)))
            ;; The positional bonus is the fractional part of the
            ;; score. We want that to come from POSITIONAL-FUTURE that
            ;; has a different MIN-TURN-TO-DEPART-1 constraint.
            (+ (round (score full-attack-future player))
               (fraction (score positional-future player)))))))

(defun eval* (planets player)
  (evaluate/full-attack planets player
                        :min-turn-to-depart-1
                        (+ *turn*
                           (if (= player 1)
                               (+ *turn* 2)
                               0))
                        :min-turn-to-depart-2
                        (+ *turn*
                           (if (= player 2)
                               (+ *turn* 2)
                               0))
                        :positional-min-turn-to-depart-1 (+ *turn* 3)))


;;;; Move generation
;;;;
;;;; A `step' is a set of orders from the same player targeting the
;;;; same planet. The constituent orders need not be for the same
;;;; turn, neither do they need to arrive on the same turn.
;;;;
;;;; A `move' is a set of orders from the same player without any
;;;; restriction. That includes future orders too.
;;;;
;;;; Move generation first computes so called step targets. A `step
;;;; target' is a ship count vector over turns representing the
;;;; desired arrivals.
;;;;
;;;; For each step target a number of steps can be found that produce
;;;; the desired arrivals. In the current implementation there is a
;;;; single step generated (see FIND-STEPS).
;;;;
;;;; Once we have the steps we want to combine them into moves. Not
;;;; all combinations are valid, but the number is combinations can be
;;;; huge. To limit the number of moves generated we first evaluate
;;;; steps one by one, sort them in descending order of evaluation
;;;; score and try to combine them starting from the first. For
;;;; details see GENERATE-MOVES-FROM-STEPS.

(defun compute-step-target (player planet future full-attack-future
                            cumulative-possible-arrivals-1
                            cumulative-possible-arrivals-2)
  (append (maybe-take-over-and-defend player planet future
                                      full-attack-future
                                      cumulative-possible-arrivals-1
                                      cumulative-possible-arrivals-2)
          (maybe-take-over-and-defend player planet future
                                      full-attack-future
                                      cumulative-possible-arrivals-1
                                      cumulative-possible-arrivals-2
                                      :surep t)
          ;; This useless playing strengthwise against this bot, but
          ;; helps against many others.
          (let ((min-turn (find-neutral-steal player future)))
            (if (and min-turn (< (growth planet) (n-ships planet)))
                (maybe-take-over-and-defend player planet future
                                            full-attack-future
                                            cumulative-possible-arrivals-1
                                            cumulative-possible-arrivals-2
                                            :min-turn-to-arrive (1+ min-turn))
                nil))))

(defun arrivals-of-player (planet player)
  (ecase player
    ((1) (arrivals-1 planet))
    ((2) (arrivals-2 planet))))

;;; Find the turn of the first ownership change in FUTURE that
;;; involves PLAYER.
(defun find-first-ownership-change (player future &key (min-turn 0))
  (let ((owners (owners future)))
    (loop for turn upfrom (max 1 min-turn) below (length owners) do
          (let ((previous-owner (aref owners (1- turn)))
                (owner (aref owners turn)))
            (when (and (/= owner previous-owner)
                       (= owner player)
                       (= previous-owner player))
              (return turn))))))

;;; For a planet with FUTURE that's not PLAYER's own on
;;; MIN-TURN-TO-ARRIVE find the earliest subsequent turn when it's
;;; possible to take it with the forces in
;;; CUMULATIVE-POSSIBLE-ARRIVALS. Return the turn on which to arrive
;;; and the required number of ships.
;;;
;;; If there is no such opportunity or if the planet is PLAYER's on
;;; MIN-TURN-TO-ARRIVE then return NIL.
(defun find-first-possible-takeover-opportunity
    (player future cumulative-possible-arrivals-1 cumulative-possible-arrivals-2
     &key (min-turn-to-arrive *turn*) surep)
  (when (= 2 player)
    (rotatef cumulative-possible-arrivals-1 cumulative-possible-arrivals-2))
  (let* ((owners (owners future))
         (n-ships-per-turn (n-ships future))
         (opponent (opponent player)))
    (loop for turn upfrom (max 1 min-turn-to-arrive) below (length owners)
          for owner = (aref owners turn)
          for n-ships = (aref n-ships-per-turn turn)
          do
          (when (/= owner player)
            (cond
              (surep
               (when (and (= 0 owner)
                          (< (1+ turn) (length owners))
                          (< n-ships (aref cumulative-possible-arrivals-1 turn))
                          (< (aref cumulative-possible-arrivals-2 (1+ turn))
                             (aref cumulative-possible-arrivals-1 turn)))
                 (return
                   (values (+ (1+ (max (aref cumulative-possible-arrivals-2
                                             (1+ turn))
                                       n-ships)))
                           turn min-turn-to-arrive)))
               (when (and (= opponent owner)
                          (< (+ n-ships
                                (aref cumulative-possible-arrivals-2 turn))
                             (aref cumulative-possible-arrivals-1 turn)))
                 (return
                   (values (1+ (+ n-ships
                                  (aref cumulative-possible-arrivals-2 turn)))
                           turn min-turn-to-arrive))))
              (t
               (when (< n-ships (aref cumulative-possible-arrivals-1 turn))
                 (return (values (1+ n-ships) turn min-turn-to-arrive)))))))))

;;; Compute the necessary arrivals for PLAYER to take over PLANET
;;; after MIN-TURN and defend it until the end. Return a
;;; vector of necessary arrivals that does not conflict with
;;; CUMULATIVE-POSSIBLE-ARRIVALS (that is, its cumulative version does
;;; not exceed CUMULATIVE-POSSIBLE-ARRIVALS at any index).
(defun maybe-take-over-and-defend (player planet future full-attack-future
                                   cumulative-possible-arrivals-1
                                   cumulative-possible-arrivals-2
                                   &key (min-turn-to-arrive *turn*) surep)
  (declare (ignore full-attack-future))
  (let ((arrivals* (make-count-vector (length (n-ships future))))
        (cheating-arrivals (make-count-vector (length (n-ships future))))
        (surplus-used 0))
    (replace arrivals* (arrivals-of-player planet player))
    ;; Take it over first if needed.
    (when (/= player (aref (owners future) min-turn-to-arrive))
      (multiple-value-bind (n-ships turn)
          (find-first-possible-takeover-opportunity
           player future cumulative-possible-arrivals-1
           cumulative-possible-arrivals-2
           :min-turn-to-arrive min-turn-to-arrive :surep surep)
        (when (and surep
                   (multiple-value-bind (n-ships* turn*)
                       (find-first-possible-takeover-opportunity
                        player future cumulative-possible-arrivals-1
                        cumulative-possible-arrivals-2
                        :min-turn-to-arrive min-turn-to-arrive)
                     (and (eql n-ships n-ships*)
                          (eql turn turn*))))
          (return-from maybe-take-over-and-defend nil))
        (when turn
          ;; When it can be taken over (or defended) we want continue
          ;; from the turn after takeover. Else why not try it
          ;; spamming it?
          (setq min-turn-to-arrive (1+ turn))
          (incf (aref cheating-arrivals turn) n-ships)
          (incf (aref arrivals* turn) n-ships)
          (incf surplus-used n-ships))))
    ;; Like RESOLVE-BATTLE but cheat in favor of PLAYER by adding just
    ;; enough ships from CUMULATIVE-POSSIBLE-ARRIVALS that PLAYER wins
    ;; every battle at or after MIN-TURN-TO-ARRIVE. Record the number
    ;; of ships required at each turn in CHEATING-ARRIVALS.
    (flet ((resolve-battle/cheat (owner c0 c1 c2 turn)
             (multiple-value-bind (new-owner new-n-ships)
                 (resolve-battle owner c0 c1 c2 turn)
               (if (and (<= min-turn-to-arrive turn)
                        (/= new-owner player)
                        (<= new-n-ships
                            (- (aref cumulative-possible-arrivals-1 turn)
                               surplus-used)))
                   (progn
                     (setf (aref cheating-arrivals turn) new-n-ships)
                     (incf (aref arrivals* turn) new-n-ships)
                     (incf surplus-used new-n-ships)
                     (values player 0))
                   (values new-owner new-n-ships)))))
      (compute-future* planet :resolve-battle-fn #'resolve-battle/cheat
                       :arrivals-1 (if (= player 1)
                                       arrivals*
                                       (arrivals-1 planet))
                       :arrivals-2 (if (= player 2)
                                       arrivals*
                                       (arrivals-2 planet))))
    (when (plusp surplus-used)
      (list cheating-arrivals))))

(defun find-neutral-steal (player future)
  (when (= 0 (first-owner future))
    (let ((owners (owners future))
          (opponent (opponent player)))
      (loop for turn upfrom *turn* below (length owners) do
            (when (= opponent (aref owners turn))
              (return turn))))))

;;; Return a `step': a set of orders targeting the planet.
;;; CUMULATIVE-SURPLUSES is a vector of cumulative surplus vectors
;;; indexed by planets id, representing the available ships.
(defun find-step (player cumulative-surpluses planet planets step-target
                  min-turn max-turn)
  (let ((used-surpluses (make-array (length planets)))
        (step ()))
    (loop for turn upfrom min-turn upto max-turn do
          (let ((need (aref step-target turn))
                (orders-this-turn ()))
            (unless (zerop need)
              (do-neighbours/reverse ((turns-to-travel neighbours) planet)
                (when (<= turns-to-travel turn)
                  (let ((turn* (- turn turns-to-travel)))
                    (dolist (neighbour neighbours)
                      (let* ((id (id neighbour))
                             (cumulative-surplus
                              (aref cumulative-surpluses id))
                             (n-ships-available
                              (- (if (= player 2)
                                     (- (aref cumulative-surplus turn*))
                                     (aref cumulative-surplus turn*))
                                 (aref used-surpluses id))))
                        (let ((n-ships (min n-ships-available need)))
                          (when (plusp n-ships)
                            (assert (<= *turn* turn*))
                            (push (make-instance 'order
                                                 :owner player
                                                 :source neighbour
                                                 :destination planet
                                                 :n-ships n-ships
                                                 :turn turn*)
                                  orders-this-turn)
                            (decf need n-ships)
                            (incf (aref used-surpluses id)
                                  n-ships))))))))
              (if (zerop need)
                  (setq step (append step orders-this-turn))
                  (return)))))
    step))

(defun find-steps (player cumulative-surpluses planet planets arrivals-needed
                   min-turn max-turn)
  (list (find-step player cumulative-surpluses planet planets arrivals-needed
                   min-turn max-turn)))

;;; Can PLAYER take the planet at any time in a full attack future?
;;;
;;; This is a slight misnomer as it also includes currently owned
;;; planets.
(defun takeablep (player full-attack-future)
  (position player (owners full-attack-future) :start *turn*))

(defun generate-candidate-steps (player game)
  (let* ((planets (planets game))
         (normal-futures (map 'vector #'compute-future planets))
         (surplus-args (list
                        ;; make it more optimistic
                        :min-turn-1 (+ *turn*
                                       (if (= player 2) 1 0))
                        :min-turn-2 (+ *turn*
                                       (if (= player 1) 1 0))
                        :include-attacked-p t))
         (cumulative-surpluses
          (map 'vector
               (lambda (future)
                 (apply #'cumulative-surplus future surplus-args))
               normal-futures))
         (surpluses (map 'vector #'uncumulate-surplus cumulative-surpluses))
         (full-attack-futures (map 'vector (lambda (planet)
                                             (compute-full-attack-future
                                              planet surpluses))
                                   planets))
         (takeablep (map 'vector (lambda (full-attack-future)
                                   (takeablep player full-attack-future))
                         full-attack-futures))
         (steps ()))
    (dotimes (i (length planets))
      (when (aref takeablep i)
        (let* ((planet (aref planets i)))
          (multiple-value-bind (cumulative-possible-arrivals-1
                                cumulative-possible-arrivals-2)
              (compute-full-attack-arrivals planet cumulative-surpluses
                                            :add-normal-arrivals-p nil
                                            :cumulativep t)
            (let ((arrivals-needed
                   (compute-step-target player planet
                                        (aref normal-futures i)
                                        (aref full-attack-futures i)
                                        cumulative-possible-arrivals-1
                                        cumulative-possible-arrivals-2)))
              (dolist (arrivals-needed arrivals-needed)
                (dolist (step (find-steps player cumulative-surpluses
                                          planet planets arrivals-needed
                                          *turn* (1- *n-turns-till-horizon*)))
                  (when step
                    (pushnew step steps :test #'move=)))))
            ;; redistribute
            (when (= 0 *depth*)
              (unless (= 0 (last-owner (aref normal-futures (id planet))))
                (loop for planet* across planets do
                      (let ((surplus (* (player-multiplier player)
                                        (aref (aref surpluses (id planet*))
                                              *turn*))))
                        (when (and (not (eq planet planet*))
                                   (= player (owner planet*))
                                   (plusp surplus)
                                   (<= (+ *turn*
                                          (turns-to-travel planet* planet))
                                       *n-turns-till-horizon*))
                          (pushnew (list
                                    (make-instance 'order
                                                   :turn *turn*
                                                   :n-ships surplus
                                                   :owner player
                                                   :destination planet
                                                   :source planet*))
                                   steps
                                   :test #'move=))))))))))
    steps))

(defun planets-involved-in-move (move)
  (let ((planets ()))
    (dolist (order move)
      (pushnew (source order) planets)
      (pushnew (destination order) planets))
    planets))

(defun valid-move-p (move)
  (handler-case
      (with-orders (move)
        (map nil #'compute-future (planets-involved-in-move move)))
    (future-impossible (f)
      (declare (ignore f))
      (return-from valid-move-p nil)))
  t)

(defun generate-moves-from-steps (steps-and-scores)
  (let ((moves (list ()))
        (n (length steps-and-scores)))
    (loop for i below (min 5 n) do
          (let ((move (first (aref steps-and-scores i))))
            (loop for j upfrom (1+ i) below n
                  for step = (first (aref steps-and-scores j)) do
                  ;; combining the empty step with anything doesn't
                  ;; make sense
                  (when step
                    (let ((move* (append move step)))
                      (when (valid-move-p move*)
                        (setq move move*)
                        (push move moves)))))))
    (nreverse moves)))

(defun score-and-sort-moves (game player moves)
  (sort (map 'vector
             (lambda (orders)
               (list orders
                     (let ((score (with-orders (orders)
                                    (eval* (planets game) player))))
                       (register-evaluated-move orders score)
                       score)))
             moves)
        #'> :key #'second))

(defun generate-and-score-moves (game player)
  ;; force caching
  (eval* (planets game) player)
  (let* ((steps (generate-candidate-steps player game))
         (sorted-steps (score-and-sort-moves game player steps))
         (composite-moves (generate-moves-from-steps sorted-steps))
         (sorted-composite-moves
          (score-and-sort-moves game player composite-moves))
         (moves (concatenate 'vector sorted-steps sorted-composite-moves)))
    (values (sort moves #'> :key #'second) sorted-steps)))


;;;; Horizon
;;;;
;;;; How far ahead the bot looks has a very strong effect on its play:
;;;; too far and it will be blind to tactics, too close and it will
;;;; miss capturing higher cost neutrals.

;;; First we calculate the `margin': the minimum (over planets)
;;; advantage of PLAYER over the opponent in cumulative possible
;;; arrivals. The margin is a ship count vector over turns.
(defun safety-margin (player planets)
  ;; FIXME: maximize for player 2
  (assert (= 1 player))
  (let* ((normal-futures (map 'vector #'compute-future planets))
         (n (length (owners (aref normal-futures 0))))
         (surplus-args (list :include-attacked-p nil))
         (cumulative-surpluses
          (map 'vector
               (lambda (future)
                 ;; OPTIMIZE: don't generate if (= 0 (LAST-OWNER
                 ;; FUTURE))
                 (apply #'cumulative-surplus future surplus-args))
               normal-futures))
         (margins (make-array n :initial-element #.(1- (expt 2 15)))))
    (loop for planet across planets
          for future across normal-futures
          do (multiple-value-bind (cumulative-possible-arrivals-1
                                   cumulative-possible-arrivals-2)
                 (compute-full-attack-arrivals planet cumulative-surpluses
                                               :add-normal-arrivals-p nil
                                               :cumulativep t)
               (safety-margin! player margins
                               (aref normal-futures (id planet))
                               cumulative-possible-arrivals-1
                               cumulative-possible-arrivals-2)))
    margins))

(defun safety-margin! (player margins future cumulative-possible-arrivals-1
                       cumulative-possible-arrivals-2)
  (loop for n-ships across (n-ships future)
        for owner across (owners future)
        for a1 across cumulative-possible-arrivals-1
        for a2 across cumulative-possible-arrivals-2
        for i upfrom 0
        do (when (and (= player owner)
                      (plusp a2))
             (setf (aref margins i)
                   (min (aref margins i)
                        (- (+ n-ships a1) a2)))))
  margins)

;;; Return the number of turns it takes for PLANET to produce at as
;;; many ships as it currently has or NIL if it's zero growth or a not
;;; a neutral.
(defun n-turns-to-break-even (planet)
  (let ((growth (growth planet)))
    (if (and (= 0 (owner planet))
             (plusp growth))
        (ceiling (n-ships planet) growth)
        nil)))

;;; It's safe to invest in a neutral planet with N-SHIPS and GROWTH
;;; that's DISTANCE turns away from the action (where ships are most
;;; needed) if we have enough margin at each turn until we break even.
(defun safe-to-invest-p (margins n-ships distance growth)
  (let ((n-turns-to-break-even (ceiling n-ships growth)))
    (loop for i upfrom 0 below (min (+ n-turns-to-break-even (* 2 distance))
                                    (length margins))
          do (when (or (minusp (aref margins i))
                       (< (aref margins i)
                          (- n-ships
                             (* growth (max 0 (- i distance))))))
               (return nil))
          finally (return t))))

;;; The horizon is normally 30 turns, but is dynamically extended to
;;; include the third least costly neutral that's safe to invest in.
(defun horizon (game)
  ;; Can only bring horizon closer.
  (let ((margins (safety-margin 1 (planets game)))
        (safe-breakeven-turns ()))
    (loop for planet across (planets game)
          do (when (and (= 0 (last-owner (compute-future planet)))
                        (plusp (growth planet)))
               (let ((n-turns-to-break-even (n-turns-to-break-even planet)))
                 (when (safe-to-invest-p margins (n-ships planet)
                                         10 (growth planet))
                   (push (+ 10 n-turns-to-break-even)
                         safe-breakeven-turns)))))
    (setq safe-breakeven-turns (sort safe-breakeven-turns #'<))
    (min *n-turns-till-horizon*
         (max 30 (or (third safe-breakeven-turns)
                     (second safe-breakeven-turns)
                     (first safe-breakeven-turns)
                     0)))))


;;;; Bocsimacko is the name of the bot.

(defclass bocsimacko ()
  (;; internally it's from 0 (not from 1 as in the engine)
   (turn :initarg :turn :initform 0 :accessor turn)
   ;; 0.8s timeout is safe on the servers
   (timeout :initform 0.8 :initarg :timeout :reader timeout)))

;;; Set up those ugly special variables.
(defmacro with-game ((game (input turn)) &body body)
  `(let* ((*n-turns-left-in-game* (- *max-n-turns* ,turn))
          (*n-turns-till-horizon* *n-turns-left-in-game*)
          ;; Must read the whole thing until `go'. We can only
          ;; hope that we don't time out.
          (,game (sb-sys:without-interrupts
                   ;; don't want the warnings
                   (let ((sb-unix::*on-dangerous-select* nil))
                     (read-game ,input))))
          (*depth* 0)
          (*turn* 0)
          (horizon (with-orders (() (length (planets ,game)))
                     (horizon ,game))))
     (when (< horizon *n-turns-till-horizon*)
       (setq *n-turns-till-horizon* horizon)
       (truncate-game game *n-turns-till-horizon*))
     (logmsg "TURN=~S, HORIZON=~S~%" ,turn *n-turns-till-horizon*)
     ;; WITH-ORDERS creates an empty move and a cache.
     (with-orders (() (length (planets game)))
       ,@body)))

(defmethod compute-orders ((bocsimacko bocsimacko) input)
  (with-best-move-on-timeout (timeout bocsimacko)
    (with-game (game (input (turn bocsimacko)))
      (when (plusp *n-turns-till-horizon*)
        (let ((n-friendly-planets
               (count 1 (planets game) :key #'owner)))
          (cond ((< n-friendly-planets 3)
                 ;; This is the opening phase. Do 4 ply
                 ;; alpha-beta with very few moves.
                 (multiple-value-bind (score actions)
                     (alpha-beta* game :widths (list 4 4 4 4))
                   (values (first actions) score)))
                (t
                 ;; After there a couple planets only do 1
                 ;; ply: just generate the moves and take the
                 ;; one with the highest score.
                 (multiple-value-bind (sorted-moves sorted-steps)
                     (generate-and-score-moves game 1)
                   (logmsg "*** all steps~%~S~%" sorted-steps)
                   (logmsg "*** all moves:~%~S~%" sorted-moves)
                   (cond ((plusp (length sorted-moves))
                          (values-list (aref sorted-moves 0)))
                         (t
                          ()))))))))))

(defmethod compute-orders :after ((bocsimacko bocsimacko) input)
  (incf (turn bocsimacko)))

;;;; Below is a block comment for code that often needs to be eval'ed
;;;; during development.

#|


(setq *verbose* t)
(setq *verbose* nil)

(sb-thread:make-thread
 (lambda ()
   (start-server-for-proxy-bot :player-class 'bocsimacko))
 :name "Proxy server")

(require :sb-sprof)

(sb-sprof:with-profiling (:loop nil :threads :all :show-progress t
                                :threads :all)
  (sleep 10))
(sb-sprof:report)

|#
