(in-package :eazy-documentation)

(defun generate-html (defs pathname &rest rest
                      &key
                        (title "(no title)")
                        (toc t)
                        (whitelist nil)
                        (blacklist '(:asdf))
                        (max-depth 2))
  (declare (ignorable title toc whitelist blacklist max-depth))
  (let ((node (apply #'generate-commondoc defs :allow-other-keys t rest)))
    (ensure-directories-exist pathname)
    (if (member (pathname-type pathname)
                '("html" "htm")
                :test 'string-equal)
        (with-open-file (s pathname
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (common-html.emitter:node-to-stream node s))
        (let ((directory (uiop:ensure-directory-pathname pathname)))
          (common-html.multi-emit:multi-emit node directory :max-depth max-depth)))
    pathname))

(defun generate-commondoc (defs
                           &key
                             (title "(no title)")
                             (toc t)
                             (whitelist nil)
                             (blacklist '(:asdf))
                             (max-depth 2))
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
        

        ;; (break)
        (ematch def
          ((class def file)
           
           ;; make a new section when the new def should not be grouped
           (when (and (not (first-iteration-p))
                      (not compatible))
             (push (make-section-from-similar-defs (reverse tmp-defs) pmode)
                   tmp-file-sections)
             (setf tmp-defs nil))

           ;; make a new section across the file boundary
           (when (and (not (first-iteration-p))
                      (not (equal file pfile)))
             (push
              (make-section (make-text
                             (pathname-name pfile)
                             :metadata (plist-hash-table '("html:class" "file")))
                            :children (reverse tmp-file-sections))
              tmp-dir-sections)
             (setf tmp-file-sections nil))
           
           ;; make a new section across the directory boundary
           (when (and (not (first-iteration-p))
                      (not (equal (pathname-directory file)
                                  (pathname-directory pfile))))
             (push
              (make-section (make-text
                             (lastcar (pathname-directory pfile))
                             :metadata (plist-hash-table '("html:class" "directory")))
                            :children (reverse tmp-dir-sections))
              tmp-sections)
             (setf tmp-dir-sections nil))
           
           (push def tmp-defs)))

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
  (make-text (string string) :metadata (plist-hash-table `("html:class" ,(format nil "~{~a~^,~}" classes)))))

(defun make-section-from-similar-defs (defs mode)
  (ecase mode
    (:missing-docs
     (make-paragraph
      (append (list (span (doctype (first-elt defs)) "doctype"))
              (iter (for def in defs)
                    (collecting
                      (span (name def) "name" (doctype def))))
              (list (span "(documentation missing)" "docstring-missing")))))
    (:shared-docstring
     (make-paragraph
      (append (list (span (doctype (first-elt defs)) "doctype"))
              (iter (for def in defs)
                    (collecting
                      (span (name def) "name" (doctype def))))
              (list (span (docstring (first-elt defs)) "docstring")))))
    (:same-name
     (make-paragraph
      (append (iter (for def in defs)
                    (collecting
                      (span (doctype def) "doctype")))
              (list (span (name (first-elt defs)) "name")
                    (span (iter (for def in defs)
                                (for docstring = (ignore-errors (docstring def)))
                                (finding docstring
                                         such-that docstring
                                         on-failure "(documentation missing)"))
                          "docstring")))))
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
