(in-package :planet-wars)



;;;; IO

(defun test-parse-planet ()
  (let ((planet (parse-planet "P 0 0 1 34 2  # Player one's home planet.")))
    (assert (= 0 (x planet)))
    (assert (= 0 (y planet)))
    (assert (= 1 (owner planet)))
    (assert (= 34 (n-ships planet)))
    (assert (= 2 (growth planet))))
  (let ((planet (parse-planet "P 7.34 -0.12 1 34 2  # comment.")))
    (assert (= 7.34 (x planet)))
    (assert (= -0.12 (y planet)))
    (assert (= 1 (owner planet)))
    (assert (= 34 (n-ships planet)))
    (assert (= 2 (growth planet)))))

(defun test-parse-fleet ()
  (let ((fleet (parse-fleet "F 1 15 0 1 12 2     # comment")))
    (assert (= 1 (owner fleet)))
    (assert (= 15 (n-ships fleet)))
    (assert (= 0 (source fleet)))
    (assert (= 1 (destination fleet)))
    (assert (= 12 (n-total-turns fleet)))
    (assert (= 2 (n-remaining-turns fleet)))))

(defun test ()
  (test-parse-planet)
  (test-parse-fleet))

(defparameter *test-game*
  "# Some example planets
P 0 0 1 34 2 # Player one's home planet.
P 7 9 2 34 2 # Player two's home planet.
P 3.14 2.71 0 15 5 # A neutral planet with real-number coordinates.

F 1 15 0 1 12 2 # Player one has sent some ships to attack player two.
F 2 28 1 2 8 4 # Player two has sent some ships to take over the neutral planet.
")

(defparameter *test-game/canonical*
  "P 0.0 0.0 1 34 2
P 7.0 9.0 2 34 2
P 3.14 2.71 0 15 5
F 1 15 0 1 12 2
F 2 28 1 2 8 4
")

(defun test-read-write-game ()
  (let* ((game (with-input-from-string (stream *test-game*)
                 (read-game stream)))
         (string
          (with-output-to-string (stream)
            (write-game game stream))))
    (prin1 string)
    (assert (string= string *test-game/canonical*))))
