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

(defun extract-definitions-from-system (system &key . #.+keywords+)
  #.+ignore+
  (when (not static-files)
    (let ((dir (asdf:system-source-directory (asdf:find-system system))))
      (when-let ((lines (append
                         (ignore-errors
                           (uiop:run-program (format nil "find ~a -name \"README*\"" dir)
                                             :output :lines))
                         (ignore-errors
                           (uiop:run-program (format nil "find ~adoc/" dir)
                                             :output :lines)))))
        (setf static-files lines))))
  (uiop:with-temporary-file (:pathname p)
    (call-with-extracting-definitions
     (lambda ()
       (dolist (file static-files)
         (ignore-errors
           (add-def :name (make-keyword (pathname-name file))
                    :doctype 'static-file
                    :file file
                    :docstring
                    (uiop:with-temporary-file (:pathname p :type "html")
                      (uiop:run-program
                       (format nil "pandoc -o ~a ~a" p file))
                      (read-file-into-string p)))))
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
  (when (not title)
    (setf (getf args :title) (format nil "~@(~a~) documentation" file)))
  (apply #'generate-commondoc
         (extract-definitions-from-file file)
         args))

(defun generate-commondoc-from-system (system &rest args &key . #.+keywords+)
  #.+ignore+
  (when (not title)
    (setf (getf args :title) (format nil "~@(~a~) documentation" system)))
  (when (not local-root)
    (setf (getf args :local-root)
          (asdf:system-source-directory (asdf:find-system system))))
  (when (not remote-root)
    (let ((hp (asdf:system-homepage (asdf:find-system system))))
      (when (search "https://github.com/" hp)
        (format nil "~a/blob/master" hp))))
  (when (not static-files)
    (let ((dir (asdf:system-source-directory (asdf:find-system system))))
      (when-let ((lines (uiop:run-program (format nil "find ~a -name \"README*\"" dir)
                                          :output :lines)))
        (setf (getf args :static-files)
              lines))))
  (apply #'generate-commondoc
         (extract-definitions-from-system system)
         args))

(defun generate-html-from-file (file pathname &rest args &key . #.+keywords+)
  #.+ignore+
  (when (not title)
    (setf (getf args :title) (format nil "~@(~a~) documentation" file)))
  (apply #'generate-html
         (extract-definitions-from-file file)
         pathname
         args))

(defun generate-html-from-system (system pathname &rest args &key loop . #.+keywords+)
  #.+ignore+
  (when (not title)
    (setf (getf args :title) (format nil "~@(~a~) documentation" system)))
  (when (not local-root)
    (setf (getf args :local-root)
          (asdf:system-source-directory (asdf:find-system system))))
  (when (not remote-root)
    (let ((hp (asdf:system-homepage (asdf:find-system system))))
      (when (search "https://github.com/" hp)
        (setf (getf args :remote-root)
              (format nil "~a/blob/master" hp)))))

  (when (not static-files)
    (let ((dir (asdf:system-source-directory (asdf:find-system system))))
      (when-let ((lines (uiop:run-program (format nil "find ~a -name \"README*\"" dir)
                                          :output :lines)))
        (setf (getf args :static-files)
              lines))))
  
  (let ((defs (extract-definitions-from-system system)))
    (if loop
        (loop
           (apply #'generate-html defs pathname args)
           (break))
        (apply #'generate-html defs pathname args))))


