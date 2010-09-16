;;;; Set up asdf locations.

(require :asdf)

(let* ((dir (pathname-directory *load-truename*))
       (asdf-files (directory
                    (merge-pathnames "**/*.asd"
                                     (make-pathname :directory dir)))))
  (setq asdf:*central-registry*
        (mapcar (lambda (directory)
                  (make-pathname :directory directory))
                (remove-duplicates (mapcar #'pathname-directory asdf-files)
                                   :test #'equal))))
