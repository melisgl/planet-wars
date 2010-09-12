;;;; http://code.google.com/p/ai-contest/wiki/GameSpecification

(in-package :planet-wars)



;;;; Model
;;;;
;;;; Speed may matter, use structs.

;;; Factor out common "base class" of BASE and FLEET so that we can
;;; get away with an empty CONC-NAME.
(defstruct (%ships (:conc-name ""))
  owner
  n-ships)

(defstruct (planet (:include %ships) (:conc-name ""))
  x
  y
  growth)

(defstruct (fleet (:include %ships) (:conc-name ""))
  source
  destination
  n-total-turns
  n-remaining-turns)

(defstruct (game (:conc-name ""))
  planets
  fleets)


;;;; IO

(defun parse-planet (line)
  (let ((tokens (split-sequence:split-sequence #\space line)))
    (assert (string= "P" (elt tokens 0)))
    (make-planet :x (parse-number:parse-number (elt tokens 1))
                 :y (parse-number:parse-number (elt tokens 2))
                 :owner (parse-number:parse-number (elt tokens 3))
                 :n-ships (parse-number:parse-number (elt tokens 4))
                 :growth (parse-number:parse-number (elt tokens 5)))))

(defun parse-fleet (line)
  (let ((tokens (split-sequence:split-sequence #\space line)))
    (assert (string= "F" (elt tokens 0)))
    (make-fleet :owner (parse-number:parse-number (elt tokens 1))
                :n-ships (parse-number:parse-number (elt tokens 2))
                :source (parse-number:parse-number (elt tokens 3))
                :destination (parse-number:parse-number (elt tokens 4))
                :n-total-turns (parse-number:parse-number (elt tokens 5))
                :n-remaining-turns (parse-number:parse-number (elt tokens 6)))))

(defun read-planet (stream)
  (parse-planet (read-line stream)))

(defun read-fleet (stream)
  (parse-fleet (read-line stream)))

(defun write-planet (planet stream)
  (format stream "P ~F ~F ~D ~D ~D~%" (x planet) (y planet)
          (owner planet) (n-ships planet) (growth planet)))

(defun write-fleet (fleet stream)
  (format stream "F ~D ~D ~D ~D ~D ~D~%" (owner fleet) (n-ships fleet)
          (source fleet) (destination fleet) (n-total-turns fleet)
          (n-remaining-turns fleet)))

(defun read-game (stream)
  (let ((planets ())
        (fleets ()))
    (loop for line = (read-line stream nil nil)
          while line
          do
          (when (plusp (length line))
            (cond ((char= #\P (aref line 0))
                   (push (parse-planet line) planets))
                  ((char= #\F (aref line 0))
                   (push (parse-fleet line) fleets)))))
    (make-game :planets (coerce (nreverse planets) 'vector)
               :fleets (coerce (nreverse fleets) 'vector))))

(defun write-game (game stream)
  (map nil (lambda (planet)
             (write-planet planet stream))
       (planets game))
  (map nil (lambda (fleet)
             (write-fleet fleet stream))
       (fleets game)))

