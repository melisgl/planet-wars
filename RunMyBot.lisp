;;;; This file can also be loaded to test a bot without compiling and
;;;; saving an image or using proxy bot. Used by bin/run-bot.sh.

;;; Load the sytem, but make sure nothing is written to the orignal
;;; stdout as that's read by the engine.
(let ((*standard-output* *error-output*))
  (load (merge-pathnames "setup.lisp" *load-truename*))
  (require :planet-wars))

(pw:play)
