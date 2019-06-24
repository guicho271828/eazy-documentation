
(in-package :common-html.emitter)

(common-doc:define-node raw-text-node (text-node) ())

;; Overall, common-html is a huge pile of hack and its API is not thoroughly
;; thought out, leading to the need for this kind of junk code
(defmethod common-html.emitter::emit ((node raw-text-node))
  (let ((text (text node)))             ; originally called plump's HTML escape code
    (if (metadata node)
        (with-tag ("span" node) (write-string text *output-stream*))
        (write-string text *output-stream*))))

(export 'raw-text-node)


(in-package :eazy-documentation)

(defun convert-file-to-html-string (pin)
  (if (string-equal "texinfo" (pathname-type pin))
      (let* ((files (uiop:run-program (format nil "grep @setfilename ~a" pin)
                                      :ignore-error-status t
                                      :output :lines))
             (output1 (second (ppcre:split " +" (first files))))
             (output2
              (make-pathname
               :type "xml"
               :name (pathname-name output1)
               :defaults (uiop:pathname-directory-pathname *target-pathname*))))
        ;; (assert (= 1 (length files)))
        (ensure-directories-exist output2)
        (uiop:run-program (format nil "cd ~a; texi2any --docbook ~a"
                                  (dirname *target-pathname*)
                                  pin)
                          :output t
                          :error-output t
                          :ignore-error-status t)
        (convert-string-to-html-string
         (read-file-into-string output2) "docbook"))
      
      (uiop:with-temporary-file (:pathname pout :type "html")
        (handler-case
            (progn
              (uiop:run-program
               (format nil "pandoc -o ~a ~a" pout pin)
               :output t
               :error-output t)
              (read-file-into-string pout))
          (uiop:subprocess-error ()
            (read-file-into-string pin))))))

(defun convert-string-to-html-string (string markup)
  (uiop:with-temporary-file (:pathname pin)
    (write-string-into-file string pin :if-exists :supersede)
    (uiop:with-temporary-file (:pathname pout :type "html")
      (handler-case
          (progn
            (uiop:run-program
             (format nil "pandoc -f ~a -o ~a ~a" markup pout pin)
             :output t
             :error-output t)
            (read-file-into-string pout))
        (uiop:subprocess-error ()
          string)))))

(defun convert-file-to-ascii-string (pin)
  (if (string-equal "texinfo" (pathname-type pin))
      (let* ((files (uiop:run-program (format nil "grep @setfilename ~a" pin)
                                      :ignore-error-status t
                                      :output :lines))
             (output1 (second (ppcre:split " +" (first files))))
             (output2
              (make-pathname
               :type "xml"
               :name (pathname-name output1)
               :defaults (uiop:pathname-directory-pathname *target-pathname*))))
        ;; (assert (= 1 (length files)))
        (ensure-directories-exist output2)
        (uiop:run-program (format nil "cd ~a; texi2any --docbook ~a"
                                  (dirname *target-pathname*)
                                  pin)
                          :output t
                          :error-output t
                          :ignore-error-status t)
        (convert-string-to-ascii-string
         (read-file-into-string output2) "docbook"))
      
      (uiop:with-temporary-file (:pathname pout :type "txt")
        (handler-case
            (progn
              (uiop:run-program
               (format nil "pandoc -o ~a ~a" pout pin)
               :output t
               :error-output t)
              (read-file-into-string pout))
          (uiop:subprocess-error ()
            (read-file-into-string pin))))))

(defun convert-string-to-ascii-string (string markup)
  (uiop:with-temporary-file (:pathname pin)
    (write-string-into-file string pin :if-exists :supersede)
    (uiop:with-temporary-file (:pathname pout :type "txt")
      (handler-case
          (progn
            (uiop:run-program
             (format nil "pandoc -f ~a -o ~a ~a" markup pout pin)
             :output t
             :error-output t)
            (read-file-into-string pout))
        (uiop:subprocess-error ()
          string)))))

