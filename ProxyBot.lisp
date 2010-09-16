;;;; This file can also be loaded to run a proxy bot without compiling
;;;; and saving an image with `make ProxyBot'.

;;; Load the sytem, but make sure nothing is written to the orignal
;;; stdout as that's read by the engine.
(let ((*standard-output* *error-output*))
  (load (merge-pathnames "setup.lisp" *load-truename*))
  (require :proxy-bot))

(proxy-bot:proxy)
