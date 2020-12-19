(in-package :eazy-documentation)

(defvar *target-pathname*)
(defvar *supported-extensions*
  '("text" "txt" "texi" "man" "docx" "epub" "md" "markdown" "tex"
    "texinfo" "wiki" "mediawiki" "org" "odt" "opml" "rst" "textile")
  "the list of supported extensions scraped by eazy-documentation")

(defun augment-args-from-file (file &rest args &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (when (not title-supplied-p)
    (setf (getf args :title) (format nil "~@(~a~) documentation" file)))
  (when (not *local-root*)
    (setf (getf args :local-root)
          (uiop:pathname-directory-pathname file)))
  (when (not *remote-root*)
    (setf (getf args :remote-root)
          (format nil "file://~a" (getf args :local-root))))
  args)

(defun strip/ (filename)
  (check-type filename string)
  (ppcre:regex-replace "/$" filename ""))

(defun augment-args-from-system (system &rest args &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (when (not title-supplied-p)
    (setf (getf args :title) (format nil "~a" system)))
  (when (not *local-root*)
    (setf (getf args :local-root)
          (asdf:system-source-directory (asdf:find-system system))))
  (when (not *remote-root*)
    (setf (getf args :remote-root)
          (if-let ((hp (asdf:system-homepage (asdf:find-system system))))
            (if (search "https://github.com/" hp)
                (format nil "~a/blob/master" hp)
                hp)
            (format nil "file://~a" (getf args :local-root)))))
  (when (not static-files)
    (let* ((dir (asdf:system-source-directory (asdf:find-system system)))
           (dir-as-string (uiop:unix-namestring dir)))
      (labels ((make-relative (filename)
                 (subseq (uiop:unix-namestring filename)
                         (length dir-as-string)))
               (ignore-p (filename)
                 ;; It would be nice to support .gitignore
                 ;; or other types of .*ignore files,
                 ;; and allow to pass additional ignore rules
                 ;; as argument.
                 ;; This way user will be able to exclude
                 ;; some files from the docs.
                 ;; 
                 ;; But right now as a hack we'll ignore just
                 ;; .qlot/ directory:
                 (match (make-relative filename)
                   ((trivia.ppcre:ppcre "^.qlot/.*")
                    t))))
        (when-let ((lines (append
                           (ignore-errors
                            (uiop:run-program
                             (format nil "find ~a -name \"README*\" -type f"
                                     ;; Without stripping last / resulting
                                     ;; pathnames will be like /Users/bob/project//other/README.md
                                     ;; and it will be harder to ignore them
                                     (strip/ dir-as-string))
                             :output :lines))
                           (ignore-errors
                            (uiop:run-program
                             (format nil "find ~adoc/ ~{-name \"*.~a\"~^ -or ~}"
                                     dir
                                     *supported-extensions*)
                             :output :lines)))))
          (setf (getf args :static-files)
                (sort (remove-if #'ignore-p
                                 lines)
                      #'string<))))))
  args)

(defun extract-definitions-from-file (file &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (uiop:with-temporary-file (:pathname p)
    (call-with-extracting-definitions
     (lambda ()
       (compile-file file :output-file p)
       (when *deferred-tasks*
         (load p)
         (mapc #'funcall *deferred-tasks*))
       *defs*))))

(defun extract-definitions-from-system (system &key . #.+keywords+)
  #.+doc+
  #.+ignore+  
  (let ((*compile-print* nil)
        (*compile-verbose* nil))
    (asdf:load-system system))
  (uiop:with-temporary-file (:pathname p)
    (call-with-extracting-definitions
     (lambda ()
       (dolist (file static-files)
         (ignore-errors
           (add-def :name (make-keyword (local-enough-namestring file))
                    :doctype 'static-file
                    :file file
                    :docstring (read-file-into-string file))))
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
  #.+doc+
  #.+ignore+
  (let* ((args (apply #'augment-args-from-file file args))
         (defs (apply #'extract-definitions-from-file file args))
         (node (apply #'generate-commondoc defs args)))
    node))

(defun generate-commondoc-from-system (system &rest args &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (let* ((args (apply #'augment-args-from-system system args))
         (defs (apply #'extract-definitions-from-system system args))
         (node (apply #'generate-commondoc defs args)))
    node))

(defun generate-html-from-file (file *target-pathname* &rest args &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (let* ((args (apply #'augment-args-from-file        file args))
         (defs (apply #'extract-definitions-from-file file args))
         (node (apply #'generate-commondoc defs args)))
    (apply #'render-html node *target-pathname* args)))

(defun generate-html-from-system (system *target-pathname* &rest args &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (let* ((args (apply #'augment-args-from-system        system args))
         (defs (apply #'extract-definitions-from-system system args))
         (node (apply #'generate-commondoc defs args)))
    (apply #'render-html node *target-pathname* args)))

(defun generate-html (file-or-system *target-pathname* &rest args &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (if (and (typep file-or-system '(or string pathname))
           (probe-file file-or-system)
           (string-equal "lisp" (pathname-type file-or-system)))
      (apply #'generate-html-from-file   file-or-system *target-pathname* args)
      (apply #'generate-html-from-system file-or-system *target-pathname* args)))
