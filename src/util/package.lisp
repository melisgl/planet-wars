(defpackage :pw-util
  (:use :cl)
  (:export #:*verbose*
           #:*log-filename*
           #:logmsg
           #:current-date-time-string
           #:backtrace-to-stream
           #:with-reckless-exit
           #:with-errors-logged
           #:fraction
           #:without-interrupts
           #:exit))
