(in-package :eazy-documentation)

;; https://fonts.googleapis.com/css?family=Comfortaa|Raleway
(defparameter *default-fonts*
  (list "Comfortaa" "Raleway" "Ubuntu")
  "A list of strings for Google font name")

(defparameter *default-css*
  (list (asdf:system-relative-pathname :eazy-documentation "default/css/default.css"))
  "A list of CSS stylesheet pathnames to be loaded in the html by default.")

(defparameter *default-js*
  (list (asdf:system-relative-pathname :eazy-documentation "default/js/default.js"))
  "A list of JavaScript pathnames to be loaded in the html by default.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +keywords+ '((title "(no title)")
                             (toc t)
                             (whitelist nil)
                             (blacklist '())
                             (max-depth 2)
                             (template-class 'eazy-template)
                             (css-list *default-css*)
                             (js-list  *default-js*)
                             (font-list  *default-fonts*)
                             (external-only t)
                             (clean nil)
                             (remote-root nil)
                             (local-root nil)
                             (static-files nil)
                             &allow-other-keys)
    "The list of keyword argument list shared by several functions.")
  (defparameter +ignore+
    `(declare (ignorable ,@(mapcar #'first (butlast +keywords+))))
    "Declare statement that says ignorable for the keyword arguments in +keywords+."))

(defun copy-destination (src dir)
  (make-pathname :name (pathname-name src)
                 :type (pathname-type src)
                 :directory (pathname-directory dir)))

(defun copy-to-dir (src dir &optional force)
  (let ((dst (copy-destination src dir)))
    (ignore-errors
      (copy-file src dst :if-to-exists (if force :supersede :error)))))

(defun basename (pathname)
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)
                 :directory nil))

(defun generate-html (defs pathname &rest args &key . #.+keywords+)
  #.+ignore+
  (let ((node (apply #'generate-commondoc defs args)))
    (ensure-directories-exist pathname)
    (if (member (pathname-type pathname) '("html" "htm") :test 'string-equal)
        ;; single file
        (let* ((directory (make-pathname :name nil :type nil :defaults pathname))
               (common-html.template:*template*
                (apply #'make-instance template-class
                       :css-list (mapcar #'basename css-list)
                       :js-list (mapcar #'basename js-list)
                       :allow-other-keys t args)))
          (dolist (src (append css-list js-list))
            (copy-to-dir src directory clean))
          (with-open-file (s pathname
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
            (common-html.emitter:node-to-stream node s)))
        ;; multi file
        (let* ((directory (uiop:ensure-directory-pathname pathname))
               (common-html.template:*template*
                (apply #'make-instance template-class
                       :css-list (mapcar #'basename css-list)
                       :js-list (mapcar #'basename js-list)
                       :allow-other-keys t args)))
          (dolist (src (append css-list js-list))
            (copy-to-dir src directory clean))
          (common-html.multi-emit:multi-emit node directory :max-depth max-depth)))
    pathname))

(defun process-black-white-list (defs blacklist whitelist external-only)
  (setf blacklist (mapcar #'find-package blacklist))
  (setf whitelist (mapcar #'find-package whitelist))
  (delete-if
   (lambda (def)
     (let ((name (safe-name def)))
       (or (some (curry #'find-symbol (symbol-name name)) blacklist)
           (and whitelist
                ;; skip if the name is not in the whitelist, if provided
                (notany (curry #'find-symbol (symbol-name name)) whitelist))
           (when external-only
             (or (null (symbol-package name)) ; for gensyms
                 (not (eq :external
                          (nth-value 1 (find-symbol (symbol-name name)
                                                    (symbol-package name))))))))))
   defs))

(defun generate-commondoc (defs &rest args &key . #.+keywords+)
  #.+ignore+
  (setf defs (process-black-white-list defs blacklist whitelist external-only))
  (let ((doc (make-document title))
        (main (apply #'generate-commondoc-main defs args)))
    (common-doc.split-paragraphs:split-paragraphs main)
    (push main (children doc))
    (when toc
      (common-doc.ops:fill-unique-refs doc)
      (push (div (make-section
                  (make-text "Index")
                  :children
                  (list (table-of-contents main :max-depth max-depth)))
                 :metadata (classes "table-of-contents"))
            (children doc)))
    (push (div (make-section (make-text title))
               :metadata (classes "title"))
          (children doc))
    doc))

(defun remote-pathname (file local-root remote-root)
  (format nil "~a/~a"
          remote-root
          (uiop:enough-pathname file local-root)))

(defun generate-commondoc-main (defs &key . #.+keywords+)
  #.+ignore+
  (iter (for def in-vector defs)
        (for pdef previous def)
        (with tmp-defs = nil)
        (with tmp-sections = nil)
        (with tmp-dir-sections = nil)
        (with tmp-file-sections = nil)

        (for pfile =
             (when (not (first-iteration-p))
               (file pdef)))
        (for (values compatible mode) =
             (when (not (first-iteration-p))
               (def~ def pdef)))
        (for pmode previous mode)
        (for i from 0)
        
        (when (not (first-iteration-p))
          (ematch def
            ((class def file)

             ;; make a new section when the new def should not be grouped
             (when (not compatible)
               (push (make-section-from-similar-defs (reverse tmp-defs) pmode)
                     tmp-file-sections)
               (setf tmp-defs nil))

             ;; make a new section across the file boundary
             (when (not (equal file pfile))
               (push
                (make-section (make-content
                               (list+ (make-text
                                       (ignore-errors (pathname-name pfile))
                                       :metadata (classes "file"))
                                      (make-text
                                       (ignore-errors (pathname-type pfile))
                                       :metadata (classes "extension"))
                                      
                                      (when pfile
                                        (if (and remote-root local-root)
                                            (make-web-link
                                             (remote-pathname pfile local-root remote-root)
                                             (list (span "[edit on web]"))
                                             :metadata (classes "source-link"))
                                            (make-web-link
                                             (format nil "file://~a" (namestring pfile))
                                             (list (span "[source]"))
                                             :metadata (classes "source-link"))))))
                              :children
                              (reverse tmp-file-sections))
                tmp-dir-sections)
               (setf tmp-file-sections nil))
             
             ;; make a new section across the directory boundary
             (when (not (equal (ignore-errors (pathname-directory file))
                               (ignore-errors (pathname-directory pfile))))
               (push
                (make-section (make-text
                               (if local-root
                                   (namestring
                                    (let ((dir (pathname-directory
                                                (uiop:enough-pathname pfile local-root))))
                                      (make-pathname
                                       :name (lastcar dir)
                                       :type nil
                                       :directory (butlast dir))))
                                   (lastcar (ignore-errors (pathname-directory pfile))))
                               :metadata (classes "directory"))
                              :children (reverse tmp-dir-sections))
                tmp-sections)
               (setf tmp-dir-sections nil)))))
           
        (push def tmp-defs)

        (finally
         (when tmp-defs
           (push
            (make-section-from-similar-defs (reverse tmp-defs) mode)
            tmp-file-sections))
         (when tmp-file-sections
           (push
            (make-section (make-text
                           (pathname-name pfile)
                           :metadata (classes "file"))
                          :children (reverse tmp-file-sections))
            tmp-dir-sections))
         (when tmp-dir-sections
           (push
            (make-section (make-text
                           (lastcar (pathname-directory pfile))
                           :metadata (classes "directory"))
                          :children (reverse tmp-dir-sections))
            tmp-sections))

         (return (div (reverse tmp-sections) :metadata (classes "main"))))))

(defun classes (&rest classes)
  (plist-hash-table
   `("html:class" ,(format nil "~{~a~^ ~}" classes))))

(defun span (string &rest classes)
  (make-text (string string)
             :metadata (when classes (apply #'classes classes))))

(defun span-id (string &rest classes)
  (make-text (string string)
             :metadata (when classes (apply #'classes classes))
             :reference (string string)))

(defun par (string &rest classes)
  (make-content (list (make-text (string string)))
                :metadata (apply #'classes classes)))

(defun div (element-or-elements &rest args)
  (apply #'make-content
         (ensure-list element-or-elements)
         args))

(defun list+ (&rest args) (remove nil (flatten args)))

(defun print-args (def)
  (ignore-errors
    (span (let ((*print-pretty* t) (*print-right-margin* 1000))
            (format nil "~(~{~a~^ ~}~)" (args def)))
          "args" "lisp")))

(defun print-package (def)
  (flet ((down (x) (string-downcase (princ-to-string x))))
    (span (down (package-name (symbol-package (safe-name def)))) "package")))

(defun make-section-from-similar-defs (defs mode)
  (flet ((down (x) (string-downcase (princ-to-string x))))
    (ecase mode
      (:same-doctype
       (div
        (make-section
         (div
          (list+ (span (down (doctype (first defs))) "doctype")
                 (span ":" "sep1")
                 (iter (for def in defs)
                       (unless (first-iteration-p)
                         (collecting
                           (span "," "sep2")))
                       (collecting
                         (span-id (down (name def)) "name" (down (doctype def)))))
                 (print-package (first defs)) 
                 (print-args (first defs))))
         :children (list+
                    (if-let ((doc (ignore-errors (docstring (first defs)))))
                      (par doc "docstring")
                      (par "(documentation missing)" "docstring" "missing"))))
        :metadata (classes "entry")))
      (:same-name
       (div
        (make-section
         (div
          (list+
           (iter (for def in defs)
                 (unless (first-iteration-p)
                   (collecting (span "," "sep2")))
                 (collecting (span (down (doctype def)) "doctype")))
           (span ":" "sep1")
           (span-id (down (name (first defs))) "name")
           (print-package (first defs))
           (print-args (first defs))))
         :children (list+
                    (if-let ((doc (ignore-errors (docstring (first defs)))))
                      (par doc "docstring")
                      (par "(documentation missing)" "docstring" "missing"))))
        :metadata (classes "entry")))
      ((nil)
       (assert (= 1 (length defs)))
       (let ((def (first defs)))
         (div
          (if (not (eq 'static-file (doctype def))) 
              (make-section
               (div
                (list+
                 (span (down (doctype def)) "doctype")
                 (span ":" "sep1")
                 (span-id (down (name def)) "name")
                 (print-package def)
                 (print-args def)))
               :children
               (list+
                (if-let ((docstring (ignore-errors (docstring def))))
                  (par docstring "docstring")
                  (par "(documentation missing)" "docstring" "missing"))))
              ;; process the static file
              (make-content
               (list
                (make-instance
                 'common-html.emitter:raw-text-node ; see 1raw-html.lisp
                 :text (docstring def)))))
          :metadata (classes "entry")))))))

(defun table-of-contents (doc-or-node &key max-depth)
  "Extract a tree of document links representing the table of contents of a
document. All the sections in the document must have references, so you should
call fill-unique-refs first.

Completely rewritten from common-html because it infers the depth incorrectly.
"
  (labels ((ol (list)
             (make-ordered-list
              (iter (for child in (remove nil list))
                    (collecting
                      (make-list-item (ensure-list child))))))
           (rec (node depth)
             #+(or) (format t "~% ~v@a ~a" depth :rec node)
             (match node
               ((section title children)
                (make-content 
                 (list* (make-document-link nil (reference node) (list title))
                        (when (< depth max-depth)
                          (ensure-list
                           (ol
                            (iter (for child in children)
                                  (appending
                                      (ensure-list (rec child (1+ depth)))))))))))
               ((content-node children)
                (iter (for child in children)
                      (appending
                          (ensure-list (rec child depth)))))
               ((document children)
                (iter (for child in children)
                      (appending
                          (ensure-list (rec child depth))))))))
    (ol (ensure-list (rec doc-or-node 1)))))


