(in-package :eazy-documentation)

(defvar *target-pathname*)
(defparameter *supported-extensions*
  '("text" "txt" "texi" "man" "docx" "epub" "md" "markdown" "tex"
    "texinfo" "wiki" "mediawiki" "org" "odt" "opml" "rst" "textile"
    ;; Man documents has numbers as their extensions:
    "1" "2" "3" "4" "5" "6" "7" "8" "9")
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


(defun collect-static-files (dir &key (name-pattern "*")
                                      (extensions *supported-extensions*))
  (check-type dir (or pathname string))
  (check-type name-pattern string)
  
  (let ((dir-as-string
          ;; Without stripping last / resulting
          ;; pathnames will be like /Users/bob/project//other/README.md
          ;; and it will be harder to ignore them
          (strip/
           (uiop:unix-namestring dir))))
    (labels ((make-relative (filename)
               (subseq (uiop:unix-namestring filename)
                       (1+ (length dir-as-string))))
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
               (let ((relative-filename (make-relative filename)))
                 (match relative-filename
                   ((trivia.ppcre:ppcre "^.qlot/.*")
                    t)))))
      (when (probe-file dir)
        (let ((lines (append
                      (uiop:run-program
                       ;; Here we use -iname for case insensitive search:
                       (format nil "find ~A -type f -iname \"~A\"~@[ \\( ~{-iname \"*.~A\"~^ -or ~} \\)~]"
                               dir-as-string
                               name-pattern
                               extensions)
                       :output :lines))))
          (sort (remove-if #'ignore-p
                           lines)
                #'string<))))))


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
    (let* ((dir (asdf:system-source-directory
                 (asdf:find-system system)))
           (docs-dir (merge-pathnames #P"doc/" dir))
           (files (append (collect-static-files dir :name-pattern "README.*")
                          (collect-static-files docs-dir))))
      (when files
        (setf (getf args :static-files)
              files))))
  args)


(defun augment-args-from-dir (dir &rest args &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (when (not title-supplied-p)
    (setf (getf args :title) (format nil "~@(~a~) documentation"
                                     (car (last (pathname-directory dir))))))
  (when (not *local-root*)
    (setf (getf args :local-root)
          (uiop:pathname-directory-pathname dir)))
  (when (not static-files)
    (setf (getf args :static-files)
          (collect-static-files dir)))
  (when (not *remote-root*)
    (setf (getf args :remote-root)
          (format nil "file://~a" (getf args :local-root))))
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


(defun extract-definitions-from-static-files (static-files)
  (unless (boundp '*defs*)
    (error "Function extract-definitions-from-static-files should be called within call-with-extracting-definitions."))
  
  (dolist (file static-files)
    ;; Probably it is not a good idea to suppress all errors
    ;; Maybe we should at least use traceback as a document body?
    ;; This will make debugging simpler.
    (ignore-errors
     (add-def :name (make-keyword (local-enough-namestring file))
              :doctype 'static-file
              :file file
              :docstring (read-file-into-string file)))))


(defun extract-definitions-from-dir (dir &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (let ((static-files (or static-files
                          (collect-static-files dir))))
    (call-with-extracting-definitions
     (lambda ()
       (extract-definitions-from-static-files static-files)
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
       (extract-definitions-from-static-files static-files)
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

(defun generate-html-from-dir (dir *target-pathname* &rest args &key . #.+keywords+)
  #.+doc+
  #.+ignore+
  (let* ((args (apply #'augment-args-from-dir dir args))
         ;; TODO: search all lisp files in dir's subdirectories.
         ;; and render them as documents.
         (defs (apply #'extract-definitions-from-dir dir args))
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
