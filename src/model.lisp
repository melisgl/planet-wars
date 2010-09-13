(in-package :planet-wars)

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

(defstruct order
  source
  destination
  n-ships)
