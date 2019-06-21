(in-package :eazy-documentation)

(defun emit-defs (defs pathname &rest rest
                  &key
                    (title "(no title)")
                    (toc t)
                    (whitelist nil)
                    (blacklist '(:asdf))
                    (max-depth 1))
  (declare (ignorable title toc whitelist blacklist max-depth))
  (let ((node (apply #'generate-doc defs :allow-other-keys t rest)))
    (if (member (pathname-type pathname)
                '("html" "htm")
                :test 'string-equal)
        (with-open-file (s pathname
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (when toc
            (write (common-html.toc:single-file-toc node :max-depth max-depth) :stream s))
          (common-html.emitter:node-to-stream node s))
        (let ((directory (uiop:ensure-directory-pathname pathname)))
          (when toc
            (with-open-file (s (merge-pathnames "index.html" directory)
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
              (write (common-html.toc:multi-file-toc node :max-depth max-depth) :stream s)))
          (common-html.multi-emit:multi-emit node directory :max-depth max-depth)))))

(defun generate-doc (defs
                     &key
                       (title "(no title)")
                       (whitelist nil)
                       (blacklist '(:asdf)))
  (iter (for def in-vector defs)
        (for pdef previous def)
        (with blacklist = (mapcar #'find-package blacklist))
        (with whitelist = (mapcar #'find-package whitelist))
        (with tmp-sections = nil)
        (with tmp-dir-sections = nil)
        (with tmp-file-sections = nil)
        (with tmp-defs = nil)

        (for pfile =
             (when (not (first-iteration-p))
               (file pdef)))
        (for (values compatible mode) =
             (when (not (first-iteration-p))
               (def~ def pdef)))
        (for pmode previous mode)
        
        (ematch def
          ((class def name file)
           
           ;; skip if the name is in the blacklist
           (when (some (curry #'find-symbol (symbol-name name)) blacklist)
             (next-iteration))

           ;; skip if the name is not in the whitelist, if provided
           (when whitelist
             (unless (some (curry #'find-symbol (symbol-name name)) whitelist)
               (next-iteration)))

           ;; make a new section across the directory boundary
           (when (and (not (first-iteration-p))
                      (not (equal (pathname-directory file)
                                  (pathname-directory pfile))))
             (push
              (make-section (make-text
                             (namestring (make-pathname :name nil :type nil :defaults pfile)))
                            :children (reverse tmp-dir-sections))
              tmp-sections)
             (setf tmp-dir-sections nil))
           
           ;; make a new section across the file boundary
           (when (and (not (first-iteration-p))
                      (not (equal file pfile)))
             (push
              (make-section (make-text (pathname-name pfile))
                            :children (reverse tmp-file-sections))
              tmp-dir-sections)
             (setf tmp-file-sections nil))

           ;; make a new section when the new def should not be grouped
           (when (and (not (first-iteration-p))
                      compatible (eq mode pmode))
             (push (make-section-from-similar-defs (reverse tmp-defs) mode)
                   tmp-file-sections)
             (setf tmp-defs nil))
           
           (push def tmp-defs)))

        (finally
         (return
           (make-document (make-text title) :children (reverse tmp-sections))))))

(defun span (string &rest classes)
  (make-text string :metadata (plist-hash-table `("html:class" ,(format nil "~{~a~^,~}" classes)))))

(defun make-section-from-similar-defs (defs mode)
  (ecase mode
    (:missing-docs
     (make-content
      (list* (span (doctype (first-elt defs)) "doctype")
             (iter (for def in defs)
                   (collecting
                     (span (name def) "name" (doctype def))))
             (span "(documentation missing)" "docstring-missing"))))
    (:shared-docstring
     (make-content
      (list* (span (doctype (first-elt defs)) "doctype")
             (iter (for def in defs)
                   (collecting
                     (span (name def) "name" (doctype def))))
             (span (docstring (first-elt defs)) "docstring"))))
    (:same-name
     (make-content
      (list* (iter (for def in defs)
                   (collecting
                     (span (doctype def) "doctype")))
             (span (name (first-elt defs)) "name")
             (span (docstring (first-elt defs)) "docstring"))))))
