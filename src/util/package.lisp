(defpackage :pw-util
  (:use :cl)
  (:export #:*verbose*
           #:*log-filename*
           #:logmsg
           #:current-date-time-string
           #:with-reckless-exit
           #:with-errors-logged
           #:fraction))
