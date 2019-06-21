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

(defun generate-commondoc-from-file (file &rest args &key (title "(no title)") (whitelist nil) (blacklist '(:asdf)))
  (declare (ignorable title whitelist blacklist))
  (apply #'generate-commondoc
         (extract-definitions-from-file file)
         args))

(defun generate-commondoc-from-system (system &rest args &key (title "(no title)") (whitelist nil) (blacklist '(:asdf)))
  (declare (ignorable title whitelist blacklist))
  (apply #'generate-commondoc
         (extract-definitions-from-system system)
         args))


(defun generate-html-from-file (file pathname &rest args
                                &key
                                  (title "(no title)")
                                  (toc t)
                                  (whitelist nil)
                                  (blacklist '(:asdf))
                                  (max-depth 1))
  (declare (ignorable title toc whitelist blacklist max-depth))
  (apply #'generate-html
         (extract-definitions-from-file file)
         pathname
         args))

(defun generate-html-from-system (system pathname &rest args
                                  &key
                                    (title "(no title)")
                                    (toc t)
                                    (whitelist nil)
                                    (blacklist '(:asdf))
                                    (max-depth 1))
  (declare (ignorable title toc whitelist blacklist max-depth))
  (apply #'generate-html
         (extract-definitions-from-system system)
         pathname
         args))
