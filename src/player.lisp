(in-package :planet-wars)

;;;; Player
;;;;
;;;; The functions below are allowed to mutate the player state. Use a
;;;; fresh instance for each game.

(defgeneric parse-game (player stream))

(defgeneric compute-orders (player))

(defgeneric game (player))


;;;; Dummy player

(defun random-elt (sequence)
  (if (> (length sequence) 0)
      (elt sequence (random (length sequence)))
      nil))

(defun enemy-planet-p (planet)
  (> (owner planet) 1))

(defun own-planet-p (planet)
  (= (owner planet) 1))

(defun neutral-planet-p (planet)
  (= (owner planet) 0))

(defun enemy-planets (game)
  (remove-if-not #'enemy-planet-p (planets game)))

(defun own-planets (game)
  (remove-if-not #'own-planet-p (planets game)))

(defun neutral-planets (game)
  (remove-if-not #'neutral-planet-p (planets game)))

(defun other-planets (game)
  (remove-if #'own-planet-p (planets game)))

(defclass dummy-player ()
  ((game :accessor game)))

(defmethod parse-game ((player dummy-player) stream)
  (setf (game player) (read-game stream)))

(defmethod compute-orders ((player dummy-player))
  (let* ((game (game player))
         (random-planet (random-elt (own-planets game))))
    (when (and random-planet
               (< 5 (n-ships random-planet)))
      (list (make-order :source random-planet
                        :destination (random-elt (other-planets game))
                        :n-ships 3)))))
