(in-package :planet-wars)

;;;; Speed may matter, use structs.

;;; Factor out common "base class" of BASE and FLEET so that we can
;;; get away with an empty CONC-NAME.
(defstruct (%ships (:conc-name ""))
  owner
  n-ships)

(defstruct (planet (:include %ships) (:conc-name ""))
  id
  x
  y
  growth
  ;; a list of fleets targeting this planet in ascending order of
  ;; N-REMAINING-TURNS
  incoming)

(defstruct (fleet (:include %ships) (:conc-name ""))
  source
  destination
  n-total-turns
  n-remaining-turns)

(defstruct (game (:conc-name ""))
  planets
  fleets)

(defstruct order
  source
  destination
  n-ships)

(defun planet-id (obj)
  (if (planet-p obj)
      (id obj)
      obj))

;;; 0 for neutral, 1 for the player, 2 for the opponent
(defparameter *max-n-players* 3)

;;; Mutates GAME.
(defun advance-time (game)
  (let* ((planets (planets game))
         (counts (make-array (list (length planets) *max-n-players*)
                             :element-type 'fixnum)))
    (loop for planet across planets do
          (unless (= 0 (owner planet))
            (incf (n-ships planet) (growth planet))
            (incf (aref counts (id planet) (owner planet)) (n-ships planet))))
    (let ((fleets ()))
      (loop for fleet across (fleets game)
            do (cond ((zerop (decf (n-remaining-turns fleet)))
                      (incf (aref counts (destination fleet) (owner fleet))
                            (n-ships fleet)))
                     (t
                      (push fleet fleets))))
      (loop for planet across planets do
            (setf (values (owner planet) (n-ships planet))
                  (resolve-battle (owner planet) (id planet) counts))))))

;;; Return the owner and the number of ships.
(defun resolve-battle (owner id counts)
  (let ((first 0)
        (second 0))
    (loop for player upfrom 1 below *max-n-players* do
          (let ((n (aref counts id player)))
            (when (<= (aref counts id second) n)
              (cond ((<= (aref counts id first) n)
                     (setq second first)
                     (setq first player))
                    (t
                     (setq second player))))))
    (if (> (aref counts id first) (aref counts id second))
        (values first (- (aref counts id first) (aref counts id second)))
        (values owner 0))))
