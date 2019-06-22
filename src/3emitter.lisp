(in-package :eazy-documentation)

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
                             (blacklist '(:asdf))
                             (max-depth 2)
                             (template-class 'eazy-template)
                             (css-list *default-css*)
                             (js-list  *default-js*)
                             &allow-other-keys)
    "The list of keyword argument list shared by several functions.")
  (defparameter +ignore+ `(declare (ignorable ,@(mapcar #'first (butlast +keywords+))))
    "Declare statement that says ignorable for the keyword arguments in +keywords+."))

(defun generate-html (defs pathname &rest args &key . #.+keywords+)
  #.+ignore+
  (let ((node (apply #'generate-commondoc defs args))
        (common-html.template:*template*
         (apply #'make-instance template-class :allow-other-keys t args)))
    (ensure-directories-exist pathname)
    (if (member (pathname-type pathname) '("html" "htm") :test 'string-equal)
        ;; single file
        (let ((directory (make-pathname :name nil :type nil :defaults pathname)))
          (dolist (src (append css-list js-list))
            ;; don't copy if it already exists
            (ignore-errors
              (copy-file src (make-pathname :name (pathname-name src)
                                            :type (pathname-type src)
                                            :directory (pathname-directory directory))
                         :if-to-exists :error)))
          (with-open-file (s pathname
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
            (common-html.emitter:node-to-stream node s)))
        ;; multi file
        (let ((directory (uiop:ensure-directory-pathname pathname)))
          (dolist (src (append css-list js-list))
            ;; don't copy if it already exists
            (ignore-errors
              (copy-file src (make-pathname :name (pathname-name src)
                                            :type (pathname-type src)
                                            :directory (pathname-directory directory))
                         :if-to-exists :error)))
          (common-html.multi-emit:multi-emit node directory :max-depth max-depth)))
    pathname))

(defun generate-commondoc (defs &key . #.+keywords+)
  (setf blacklist (mapcar #'find-package blacklist))
  (setf whitelist (mapcar #'find-package whitelist))

  (setf defs
        (delete-if
         (lambda (def)
           (ematch def
             ((class def name)
              ;; skip if the name is in the blacklist
              (or (some (curry #'find-symbol (symbol-name name)) blacklist)
                  (and whitelist
                       ;; skip if the name is not in the whitelist, if provided
                       (notany (curry #'find-symbol (symbol-name name)) whitelist))))))
         defs))
  
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
                (make-section (make-text
                               (pathname-name pfile)
                               :metadata (plist-hash-table '("html:class" "file")))
                              :children (reverse tmp-file-sections))
                tmp-dir-sections)
               (setf tmp-file-sections nil))
             
             ;; make a new section across the directory boundary
             (when (not (equal (pathname-directory file)
                               (pathname-directory pfile)))
               (push
                (make-section (make-text
                               (lastcar (pathname-directory pfile))
                               :metadata (plist-hash-table '("html:class" "directory")))
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
                           :metadata (plist-hash-table '("html:class" "file")))
                          :children (reverse tmp-file-sections))
            tmp-dir-sections))
         (when tmp-dir-sections
           (push
            (make-section (make-text
                           (lastcar (pathname-directory pfile))
                           :metadata (plist-hash-table '("html:class" "directory")))
                          :children (reverse tmp-dir-sections))
            tmp-sections))

         (let ((doc (make-document title :children (reverse tmp-sections))))
           (when toc
             (common-doc.ops:fill-unique-refs doc)
             (push (make-section
                    (make-text "Index")
                    :children
                    (list (common-doc.ops:table-of-contents doc :max-depth max-depth)))
                   (children doc)))
           (return doc)))))

(defun span (string &rest classes)
  (make-text (string string)
             :metadata
             (plist-hash-table
              `("html:class" ,(format nil "~{~a~^,~}" classes)))))



(defun make-section-from-similar-defs (defs mode)
  (ecase mode
    (:missing-docs
     (make-paragraph
      `(,(span (doctype (first-elt defs)) "doctype")
         ,@(iter (for def in defs)
                 (collecting
                   (span (name def) "name" (doctype def))))
         ,(span "(documentation missing)" "docstring-missing"))))
    (:shared-docstring
     (make-paragraph
      `(,(span (doctype (first-elt defs)) "doctype")
         ,@(iter (for def in defs)
                 (collecting
                   (span (name def) "name" (doctype def))))
         ,(span (docstring (first-elt defs)) "docstring"))))
    (:same-name
     (make-paragraph
      `(,@(iter (for def in defs)
                (collecting
                  (span (doctype def) "doctype")))
          ,(span (name (first-elt defs)) "name")
          ,(span (iter (for def in defs)
                       (for docstring = (ignore-errors (docstring def)))
                       (finding docstring
                                such-that docstring
                                on-failure "(documentation missing)"))
                 "docstring"))))
    ((nil)
     (assert (= 1 (length defs)))
     (make-paragraph
      (iter (for def in defs)
            (collecting
              (span (doctype def) "doctype"))
            (collecting
              (span (name def) "name"))
            (collecting
              (span (or (ignore-errors (docstring def))
                        "(documentation missing)")
                    "docstring")))))))
