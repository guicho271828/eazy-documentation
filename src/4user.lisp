(in-package :eazy-documentation)

(defun extract-definitions-from-file (file)
  (uiop:with-temporary-file (:pathname p)
    (call-with-extracting-definitions
     (lambda ()
       (compile-file file :output-file p)
       (when *deferred-tasks*
         (load p)
         (mapc #'funcall *deferred-tasks*))
       *defs*))))

(defun extract-definitions-from-system (system)
  (uiop:with-temporary-file (:pathname p)
    (call-with-extracting-definitions
     (lambda ()
       (with-compilation-unit ()
         (let ((*compile-print* nil)
               (*compile-verbose* nil))
           (let ((*package* (find-package :asdf)))
             (compile-file (asdf:system-source-file system) :output-file p))
           (asdf:clear-system system)
           (asdf:compile-system system :verbose nil :force t)))
       (when *deferred-tasks*
         (asdf:load-system system)
         (mapc #'funcall *deferred-tasks*))
       *defs*))))

;; This is not a magic: +keywords+ is a list, and I am just inserting it in the
;; read time

(defun generate-commondoc-from-file (file &rest args &key . #.+keywords+)
  #.+ignore+
  (when (not title-given-p)
    (setf (getf args :title) (format nil "~a documentation" file)))
  (apply #'generate-commondoc
         (extract-definitions-from-file file)
         args))

(defun generate-commondoc-from-system (system &rest args &key . #.+keywords+)
  #.+ignore+
  (when (not title-given-p)
    (setf (getf args :title) (format nil "~a documentation" system)))
  (apply #'generate-commondoc
         (extract-definitions-from-system system)
         args))

(defun generate-html-from-file (file pathname &rest args &key . #.+keywords+)
  #.+ignore+
  (when (not title-given-p)
    (setf (getf args :title) (format nil "~a documentation" file)))
  (apply #'generate-html
         (extract-definitions-from-file file)
         pathname
         args))

(defun generate-html-from-system (system pathname &rest args &key . #.+keywords+)
  #.+ignore+
  (when (not title-given-p)
    (setf (getf args :title) (format nil "~a documentation" system)))
  (apply #'generate-html
         (extract-definitions-from-system system)
         pathname
         args))
